delimiter //



DROP PROCEDURE IF EXISTS DisableDiscount;
CREATE PROCEDURE DisableDiscount (IN id INT)
sproc:BEGIN

DECLARE priceWithoutDiscount INT;

SELECT list_price into priceWithoutDiscount from SS_products WHERE productID = id; 
UPDATE SS_products SET Price = priceWithoutDiscount WHERE productID = id;


END;
