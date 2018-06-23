select points.rowid id, points.*
 from points, tracks, trips
  where points.track_id = tracks.rowid
    and tracks.trip_id = :trip_id
    and trips.rowid = tracks.trip_id
     order by tracks.trip_rn, tracks.rn
