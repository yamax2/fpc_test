create table if not exists sessions(
  id text not null,
  crc32 text,
  cc integer not null default 0,
  loaded integer not null default 0,

  primary key(id)
);

create table if not exists tracks(
  session_id text not null,
  rn integer not null default 0,
  filename text not null, 
  created_at text not null,
  size integer not null default 0,
  trip_id integer,
  trip_rn integer,
  start_id integer,
  end_id integer
);

create table if not exists points(
  track_id integer not null,
  lat real,
  lon real,
  time text,
  course real,
  speed real,
  type text not null
);

create table if not exists trips(
  session_id text not null,
  started_at text not null,    -- trip started at
  duration integer not null,   -- duration, sec
  distance real not null,      -- distance, km
  avg_speed real not null,     -- avg speed km/h
  size integer not null        -- size of all files in bytes
);

create index if not exists ix_points_track_id on points(track_id);
create index if not exists ix_tracks_session_id on tracks(session_id);
create index if not exists ix_tracks_trip_id on tracks(trip_id);
create index if not exists ix_tracks_trip on tracks(trip_id, rn);
create index if not exists ix_trips_session_id on trips(session_id);
create index if not exists ix_tracks_start_id on tracks(start_id);
create index if not exists ix_tracks_end_id on tracks(end_id);
