SELECT
  fish_observation_point_id,
  s.gradient,
  s.stream_order,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM whse_fish.fiss_fish_obsrvtn_events_vw e
INNER JOIN whse_basemapping.fwa_stream_networks_sp s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.species_code = 'GR'
AND e.watershed_group_code = 'PARS';
