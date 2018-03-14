-- select load_extension('/projects/fpc_test/sqlite/libsqlitefunctions.so');


with info as (
select points.track_id, 
       min(points.rowid) start_id, 
	   max(points.rowid) end_id 
 from points
  join tracks on tracks.rowid = points.track_id
    where tracks.session_id = '9e7bcf98-a74d-4aa5-b485-658561b236b7' 
	  and points.type = 'A'
      group by track_id),
	  
source as (
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
  from source s1, source s2
   where s1.track_id <> s2.track_id
    and s2.start_time >= s1.end_time
),

results as (
select rr.t1_id, 
       rr.t2_id,  
	   6371000 * 2 * atan2(sqrt(rr.a), sqrt(1 - rr.a)) sd,
	   td
from rr
)


select r.*
  from results r