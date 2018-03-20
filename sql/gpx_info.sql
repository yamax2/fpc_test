with bounds as (
select min(points.lat) minlat, min(points.lon) minlon,
       max(points.lat) maxlat, max(points.lon) maxlon
  from tracks, points 
 where tracks.trip_id = :trip_id
   and points.track_id = tracks.rowid
   and points.type = 'A'
)
select trips.rowid id,
       started_at,
       duration,
       distance,
       avg_speed,
       bounds.*
 from trips, bounds
   where trips.rowid = :trip_id
