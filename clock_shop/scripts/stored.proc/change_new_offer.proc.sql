delimiter //



DROP PROCEDURE IF EXISTS ChangeNewOffer;
CREATE PROCEDURE ChangeNewOffer (IN newID INT, IN newPrice INT)
sproc:BEGIN

DECLARE oldID INT;
DECLARE priceWithoutDiscount INT;

SELECT productID into oldID from SS_new_offers;
SELECT list_price into priceWithoutDiscount from SS_products WHERE productID = oldID; 


UPDATE SS_products SET Price = priceWithoutDiscount WHERE productID = oldID;
UPDATE SS_products SET Price = newPrice WHERE productID = newID; 
 
UPDATE SS_new_offers SET productID = newID WHERE productID = oldID; 



END;
