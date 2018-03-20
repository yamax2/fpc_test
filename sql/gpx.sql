select points.rowid id,
       points.lat, 
       points.lon,
       points.time,
       points.course,
       points.speed
  from tracks, points 
    where tracks.trip_id = :trip_id
      and points.track_id = tracks.rowid
	   order by tracks.rn, points.rn
