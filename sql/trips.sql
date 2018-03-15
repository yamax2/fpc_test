with recursive limits as (
select 60 * 60 time_limit,
       1000 m_limit,
       '01c40520-e416-4f7a-8b98-0bdc7613e3ff' session_id
),
info as (
-- track info: start point, end_point
select points.track_id,
       min(points.rowid) start_id,
       max(points.rowid) end_id
 from limits, points
  join tracks on tracks.rowid = points.track_id
    where tracks.session_id = limits.session_id
      and points.type = 'A'
      group by track_id),
source as (
-- source info: track start and end coords and time
select info.track_id,

       radians(p1.lat) start_latr,
       radians(p1.lon) start_lonr,

       strftime('%s', p1.time) start_time,

       radians(p2.lat) end_latr,
       radians(p2.lon) end_lonr,

       strftime('%s', p2.time) end_time
 from info
   join points p1 on p1.rowid = info.start_id
   join points p2 on p2.rowid = info.end_id
),
rr as (
select s1.track_id t1_id,
       s2.track_id t2_id,

       power(sin( (s2.start_latr - s1.end_latr) / 2), 2) +
         cos(s1.start_latr) * cos(s2.end_latr) * power(sin((s2.start_lonr - s1.end_lonr) / 2), 2) a,

       s2.start_time - s1.end_time td
  from source s1, source s2, limits
   where s1.track_id <> s2.track_id
     and s2.start_time >= s1.end_time
     and s2.start_time - s1.end_time <= limits.time_limit
),
dd as (
select rr.t1_id,
       rr.t2_id,
       6371000 * 2 * atan2(sqrt(rr.a), sqrt(1 - rr.a)) m_delta,
       rr.td time_delta
from rr, limits
  where 6371000 * 2 * atan2(sqrt(rr.a), sqrt(1 - rr.a)) <= limits.m_limit
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
 from limits, tracks t
   join dd2 on dd2.t1_id = t.rowid
    where t.session_id = limits.session_id
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
select * from zz order by lv, rn
