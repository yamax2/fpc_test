select trip_rn,
       filename,
       start_id,
       end_id
from tracks
 where trip_id = :trip_id
  order by trip_rn
