
BEGIN;
-- GET CONVIENIENT DATA
SELECT o.id as offer_id,
       p.epid as epid,
       o.merchant_id AS merchant_id,
       CASE o.price_in_usd_cents > 0
         WHEN true THEN o.price_in_usd_cents
         ELSE NULL
        END AS offer_price
  INTO TEMPORARY TABLE active_offers
  FROM pluto_dev.shopping_product p,
       pluto_dev.shopping_offer o
 WHERE p.id = o.product_id AND
       p.preload_rtpal = true AND
       p.epid IS NOT NULL AND
       o.page_status = 0;

CREATE INDEX active_offers_ix on active_offers (offer_id);

SELECT DISTINCT(ao.merchant_id) as merchant_id
  INTO TEMPORARY TABLE active_merchants
  FROM active_offers ao;

CREATE INDEX active_merchants_ix on active_merchants (merchant_id);

SELECT ma.id as merchant_address_id
  INTO TEMPORARY TABLE active_stores
  FROM pluto_dev.merchant_addresses ma
 WHERE ma.merchant_id IN (SELECT merchant_id FROM active_merchants);

CREATE INDEX active_stores_ix on active_stores (merchant_address_id);



-- DELETE
DELETE FROM deploy_to_ebay.zip_codes dz
 WHERE dz.zip_code NOT IN (SELECT zip_code FROM zipdb.zip_lat_lng_pairs);

DELETE FROM deploy_to_ebay.availabilities da
 WHERE da.merchant_address_id NOT IN (SELECT merchant_address_id FROM active_stores);

DELETE FROM deploy_to_ebay.availabilities da
 WHERE da.offer_id NOT IN (SELECT offer_id FROM active_offers);

DELETE FROM deploy_to_ebay.offers
 WHERE offer_id NOT IN (SELECT offer_id FROM active_offers);

DELETE FROM deploy_to_ebay.stores ds
 WHERE ds.merchant_id NOT IN (SELECT merchant_id FROM active_merchants);

DELETE FROM deploy_to_ebay.merchants
 WHERE merchant_id NOT IN (SELECT merchant_id FROM active_merchants);


-- UPDATE

UPDATE deploy_to_ebay.zip_codes uz
   SET latitude=z.latitude,
       longitude=z.longitude
  FROM zipdb.zip_lat_lng_pairs z
 WHERE z.zip_code = uz.zip_code AND
       (coalesce(uz.latitude != z.latitude, true) OR
        coalesce(uz.longitude != z.longitude, true));

UPDATE deploy_to_ebay.merchants
   SET name = m.name,
       icon_url = 'http://static.milo.com' ||
                  '/20110202203218/images/store_icons/flat_by_id/' ||
                   m.id || '.png'
  FROM pluto_dev.shopping_merchant m
 WHERE m.id = deploy_to_ebay.merchants.merchant_id;

UPDATE deploy_to_ebay.offers uo
   SET url = o.url
  FROM pluto_dev.shopping_offer o
 WHERE o.id = uo.offer_id AND
       coalesce(o.url != uo.url, true);

UPDATE deploy_to_ebay.stores us
   SET merchant_id = ma.merchant_id,
       store_id = ma.store_id,
       descriptor = ma.descriptor,
       street = ma.street,
       city = ma.city,
       state = ma.region,
       zip_code = ma.zip_code,
       latitude = ma.lat,
       longitude = ma.lng,
       phone = ma.phone,
       hours = ma.hours
  FROM pluto_dev.merchant_addresses ma
 WHERE us.merchant_address_id = ma.id AND
       (coalesce(us.merchant_id != ma.merchant_id, true) OR
        coalesce(us.store_id != ma.store_id, true) OR
        coalesce(us.descriptor != ma.descriptor, true) OR
        coalesce(us.street != ma.street, true) OR
        coalesce(us.city != ma.city, true) OR
        coalesce(us.state != ma.region, true) OR
        coalesce(us.zip_code != ma.zip_code, true) OR
        coalesce(us.latitude != ma.lat, true) OR
        coalesce(us.longitude != ma.lng, true) OR
        coalesce(us.phone != ma.phone, true) OR
        coalesce(us.hours != ma.hours, true));


UPDATE deploy_to_ebay.availabilities ua
   SET epid = ao.epid,
       offer_price = ao.offer_price
  FROM active_offers ao
 WHERE ua.offer_id = ao.offer_id AND
       (coalesce(ua.epid != ao.epid, true) OR
        coalesce(ua.offer_price != ao.offer_price, true));

-- INSERT

INSERT INTO deploy_to_ebay.zip_codes (zip_code, latitude, longitude)
 (SELECT zip_code, latitude, longitude
    FROM zipdb.zip_lat_lng_pairs
   WHERE latitude IS NOT NULL AND
         longitude IS NOT NULL AND
         zip_code NOT IN (SELECT zip_code FROM deploy_to_ebay.zip_codes));

INSERT INTO deploy_to_ebay.merchants (merchant_id,
                                      name,
                                      icon_url)
    (SELECT m.id,
            m.name,
            'http://static.milo.com' ||
            '/20110202203218/images/store_icons/flat_by_id/' ||
            m.id || '.png'
       FROM pluto_dev.shopping_merchant m
      WHERE m.id IN (SELECT merchant_id FROM active_merchants
                       EXCEPT
                     SELECT merchant_id FROM deploy_to_ebay.merchants));

INSERT INTO deploy_to_ebay.offers (offer_id, url)
     (SELECT pdo.id,
             pdo.url
        FROM pluto_dev.shopping_offer pdo
       WHERE pdo.id IN (SELECT offer_id FROM active_offers
                          EXCEPT
                        SELECT offer_id FROM deploy_to_ebay.offers));

INSERT INTO deploy_to_ebay.stores (merchant_address_id,
                                   merchant_id,
                                   store_id,
                                   descriptor,
                                   street,
                                   city,
                                   state,
                                   zip_code,
                                   latitude,
                                   longitude,
                                   phone,
                                   hours)
    (SELECT ma.id,
            ma.merchant_id,
            ma.store_id,
            ma.descriptor,
            ma.street,
            ma.city,
            ma.region,
            ma.zip_code,
            ma.lat,
            ma.lng,
            ma.phone,
            ma.hours
      FROM pluto_dev.merchant_addresses ma
      WHERE ma.id IN (SELECT merchant_address_id FROM active_stores
                        EXCEPT
                      SELECT merchant_address_id FROM deploy_to_ebay.stores));


INSERT INTO deploy_to_ebay.availabilities (as_of,
                                           epid,
                                           product_id,
                                           offer_id,
                                           merchant_address_id,
                                           status,
                                           offer_price)

    (SELECT now(),
            ao.epid,
            so.product_id,
            ao.offer_id,
            ma.id,
            'error',
            CASE so.price_in_usd_cents > 0
              WHEN true THEN so.price_in_usd_cents
              ELSE NULL
            END
      FROM active_offers ao,
           pluto_dev.shopping_offer so,
           pluto_dev.merchant_addresses ma
      WHERE so.merchant_id = ma.merchant_id AND
            ao.offer_id = so.id AND
            NOT EXISTS (SELECT 1
                          FROM deploy_to_ebay.availabilities a
                         WHERE a.offer_id = ao.offer_id AND
                               a.merchant_address_id = ma.id));

COMMIT;
