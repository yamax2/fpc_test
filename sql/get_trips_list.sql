select rowid id, trips.*, size * 1.0 / (1024 * 1024) size_mb from trips
  where session_id = :session_id
   order by start_id
