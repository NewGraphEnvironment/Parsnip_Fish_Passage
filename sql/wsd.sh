curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359566405?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 57690.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359518418?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 57695.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359477394?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 57696.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359364008?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 125098.geojson

curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359394633?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 125175.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359190012?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 125186.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359502014?downstream_route_measure=20&srid=3005" -H "accept: application/json" > 125231.geojson

# manually extracted blkey
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359550412?downstream_route_measure=20&srid=3005" -H "accept: application/json" > CV1.geojson

# tweaked measures
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359570734?downstream_route_measure=200&srid=3005" -H "accept: application/json" > 125345.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359532981?downstream_route_measure=400&srid=3005" -H "accept: application/json" > 125403.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359513994?downstream_route_measure=100&srid=3005" -H "accept: application/json" > 57681.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359485108?downstream_route_measure=1300&srid=3005" -H "accept: application/json" > 125253.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359527248?downstream_route_measure=100&srid=3005" -H "accept: application/json" > 125247.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359158983?downstream_route_measure=100&srid=3005" -H "accept: application/json" > 125180.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359360377?downstream_route_measure=500&srid=3005" -H "accept: application/json" > 125179.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359525341?downstream_route_measure=580&srid=3005" -H "accept: application/json" > 125128.geojson
curl -X GET "https://hillcrestgeo.ca/fwa/v1/watershed/359438783?downstream_route_measure=2412&srid=3005" -H "accept: application/json" > 125000.geojson

geojson-merge *geojson > t.json
ogr2ogr wsds.shp -s_srs EPSG:3005 -t_srs EPSG:3005  t.json