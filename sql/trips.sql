-- select load_extension('/projects/fpc_test/sqlite/libsqlitefunctions.so')

create temporary table tmp_consts as 
select 20 * 60 time_limit, -- max time between trips
       1000 m_limit,       -- max distance between trips
       :session_id session_id;

with rns as (
select points.track_id,
       min(points.rn) start_rn,
       max(points.rn) end_rn
 from tmp_consts consts, points
  join tracks on tracks.rowid = points.track_id
    where tracks.session_id = consts.session_id
      and points.type = 'A'
      group by track_id
),
info as (
select rns.track_id,
       p1.rowid start_id,
       p2.rowid end_id
 from rns 
  join points p1 on p1.track_id = rns.track_id and p1.rn = rns.start_rn
  join points p2 on p2.track_id = rns.track_id and p2.rn = rns.end_rn
)
update tracks 
   set start_id = (select start_id from info where info.track_id = tracks.rowid),
       end_id = (select end_id from info where info.track_id = tracks.rowid)
where session_id in (select session_id from tmp_consts);

create temporary table tmp_distances as
with source1 as (
select tracks.rowid track_id,
       radians(p1.lat) start_latr,
       radians(p1.lon) start_lonr,
       radians(p2.lat) end_latr,
       radians(p2.lon) end_lonr
 from tracks, tmp_consts consts
  join points p1 on p1.track_id = tracks.rowid
  join points p2 on p2.track_id = tracks.rowid and p2.rn = p1.rn + 1
   where tracks.session_id = consts.session_id 
     and p1.type = 'A'
     and p2.type = 'A'
),
source2 as (
select track_id,
       power(sin( (end_latr - start_latr) / 2), 2) +
         cos(start_latr) * cos(end_latr) * power(sin((end_lonr - start_lonr) / 2), 2) a
 from source1
)
select track_id, sum(6371000 * 2 * atan2(sqrt(a), sqrt(1 - a))) distance
 from source2
   group by track_id;

update tracks 
   set distance = coalesce((select distance from tmp_distances where tmp_distances.track_id = tracks.rowid), 0)
where session_id in (select session_id from tmp_consts);

create temporary table tmp_trips as
with recursive source as (
-- source info: track start and end coords and time
select info.rowid track_id,

       radians(p1.lat) start_latr,
       radians(p1.lon) start_lonr,

       strftime('%s', p1.time) start_time,

       radians(p2.lat) end_latr,
       radians(p2.lon) end_lonr,

       strftime('%s', p2.time) end_time
 from tracks info
   join points p1 on p1.rowid = info.start_id
   join points p2 on p2.rowid = info.end_id
),
rr as (
select s1.track_id t1_id,
       s2.track_id t2_id,

       power(sin( (s2.start_latr - s1.end_latr) / 2), 2) +
         cos(s1.start_latr) * cos(s2.end_latr) * power(sin((s2.start_lonr - s1.end_lonr) / 2), 2) a,

       s2.start_time - s1.end_time td
  from source s1, source s2, tmp_consts consts
   where s1.track_id <> s2.track_id
     and s2.start_time >= s1.end_time
     and s2.start_time - s1.end_time <= consts.time_limit
),
dd as (
select rr.t1_id,
       rr.t2_id,
       6371000 * 2 * atan2(sqrt(rr.a), sqrt(1 - rr.a)) m_delta,
       rr.td time_delta
from rr, tmp_consts consts
  where 6371000 * 2 * atan2(sqrt(rr.a), sqrt(1 - rr.a)) <= consts.m_limit
),
dd1 as (
select t1_id, min(time_delta)  min_time_delta
 from dd
  group by t1_id
),
dd2 as (
/*
select distinct t1_id,
       first_value(t2_id) over (partition by t1_id order by td) t2_id
 from dd
*/
select dd1.t1_id,
       (select min(t2_id) from dd where t1_id = dd1.t1_id and time_delta = dd1.min_time_delta) t2_id
  from dd1
),
rrs as (
select t.rowid id, dd2.t2_id
 from tmp_consts consts, tracks t
   join dd2 on dd2.t1_id = t.rowid
    where t.session_id = consts.session_id
),
tops as (
select id from rrs r
 where not exists(select null from rrs rr where rr.t2_id = r.id)
),
zz as (
select id, tops.id lv, 1 rn from tops
union all
select rrs.t2_id, zz.lv, zz.rn + 1
 from zz, rrs
   where rrs.id = zz.id and rrs.t2_id is not null
)
select consts.session_id, zz.* from zz, tmp_consts consts
union all
select consts.session_id, tracks.rowid, tracks.rowid, 1 
 from tmp_consts consts, tracks 
  where tracks.rowid not in (select id from zz)
    and tracks.session_id = consts.session_id;

insert into trips(session_id, start_id, started_at, duration, size, distance, avg_speed)
with r1 as (
select t.session_id, 
       t.lv start_id,
       max(t.rn) max_rn
  from tmp_trips t
   group by t.session_id, t.lv
),
r2 as (
select r1.session_id,
       r1.start_id,
       (select id from tmp_trips
          where session_id = r1.session_id
            and lv = r1.start_id
            and rn = r1.max_rn) end_id
 from r1
)
select r2.session_id,
       r2.start_id lv,

       p1.time started_at,
       strftime('%s', p2.time) - strftime('%s', p1.time) duration,

       (select sum(tracks.size)
          from tracks, tmp_trips
            where tmp_trips.lv = r2.start_id and tracks.rowid = tmp_trips.id) size,

       (select sum(tracks.distance)
          from tmp_trips, tracks
         where tmp_trips.lv = r2.start_id
           and tracks.rowid = tmp_trips.id
       ) distance,

       (select avg(points.speed)
          from tracks, tmp_trips, points 
         where tmp_trips.lv = r2.start_id 
           and tracks.rowid = tmp_trips.id
           and points.track_id = tracks.rowid
           and points.speed > 0
       ) avg_speed
  from r2
   join tracks t1 on t1.rowid = r2.start_id
   join points p1 on p1.rowid = t1.start_id

   join tracks t2 on t2.rowid = r2.end_id
   join points p2 on p2.rowid = t2.end_id;

update tracks
   set trip_id = (
          select trips.rowid
            from trips, tmp_trips t
           where trips.session_id = tracks.session_id
             and t.id = tracks.rowid
             and t.lv = trips.start_id
       ),

       trip_rn = (
         select t.rn
           from tmp_trips t
          where t.id = tracks.rowid 
            and t.session_id = tracks.session_id
       )
where session_id in (select session_id from tmp_consts);

drop table tmp_distances;
drop table tmp_consts;
drop table tmp_trips;
