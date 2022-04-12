-- Run this in the OCA Level-2 database
SELECT
    i.indexnumberid,
    i.court,
    i.fileddate,
    i.propertytype,
    i.status,
    a.street1 AS street,
    a.city,
    LEFT (a.postalcode, 5) AS zipcode,
    a.lat,
    a.lon
FROM
    oca_index AS i
    LEFT JOIN oca_addresses AS a USING (indexnumberid)
WHERE
    -- only eviction cases
    i.classification = ANY ('{Holdover,Non-Payment}')
    -- only active cases
    AND i.status ~ 'Active'
    -- we can only distinguish res/comm inside NYC, so remove comm cases where possible
    AND ((propertytype = 'Residential'
            AND court = ANY ('{
					Bronx County Civil Court,
					Kings County Civil Court,
					New York County Civil Court,
					Queens County Civil Court,
					Richmond County Civil Court,
					Redhook Community Justice Center,
					Harlem Community Justice Center
				}'))
        OR NOT (court = ANY ('{
					Bronx County Civil Court,
					Kings County Civil Court,
					New York County Civil Court,
					Queens County Civil Court,
					Richmond County Civil Court,
					Redhook Community Justice Center,
					Harlem Community Justice Center
				}')))
ORDER BY
    fileddate ASC;

