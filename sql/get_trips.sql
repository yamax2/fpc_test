select rowid id, trips.* from trips
 where session_id = :session_id
