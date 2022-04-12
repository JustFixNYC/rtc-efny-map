-- Run this in the JustFix Admin Dashboard
SELECT
  efny.created_at::date,
  efny.lob_letter_object #> '{from_address}' ->> 'address_line1' AS street,
  efny.lob_letter_object #> '{from_address}' ->> 'address_city' AS city,
  efny.lob_letter_object #> '{from_address}' ->> 'address_zip' AS zipcode,
  efny.declaration_variables ->> 'address' AS address
FROM
  evictionfree_submittedhardshipdeclaration AS efny
