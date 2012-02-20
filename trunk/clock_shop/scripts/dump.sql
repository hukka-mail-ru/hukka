-- MySQL dump 10.13  Distrib 5.1.41, for debian-linux-gnu (i486)
--
-- Host: localhost    Database: ClockShop
-- ------------------------------------------------------
-- Server version	5.1.41-3ubuntu12.10

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `ClockShop`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `ClockShop` /*!40100 DEFAULT CHARACTER SET utf8 */;

USE `ClockShop`;

--
-- Table structure for table `SS_categories`
--

DROP TABLE IF EXISTS `SS_categories`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_categories` (
  `categoryID` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `name_en` varchar(255) DEFAULT NULL,
  `name_de` varchar(255) DEFAULT NULL,
  `parent` int(11) DEFAULT NULL,
  `products_count` int(11) DEFAULT NULL,
  `description` text,
  `description_en` text,
  `description_de` text,
  `picture` varchar(255) DEFAULT NULL,
  `products_count_admin` int(11) DEFAULT NULL,
  PRIMARY KEY (`categoryID`)
) ENGINE=MyISAM AUTO_INCREMENT=79 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_categories`
--

LOCK TABLES `SS_categories` WRITE;
/*!40000 ALTER TABLE `SS_categories` DISABLE KEYS */;
INSERT INTO `SS_categories` VALUES (1,'Часы с маятником',NULL,'Pendeluhren',0,0,'Расставьте акценты в Вашей комнате! Настенные часы с маятником - это аксессуар, способный подчеркнуть оригинальность интерьера вашей комнаты или кухни.',NULL,NULL,'category/00.2227.00.jpg',0),(2,'Часы круглые    ',NULL,'Rundbogen',0,0,'description',NULL,NULL,'category/03.2783.00.jpg',0),(3,'Часы овальные   ',NULL,'Wanduhren',0,0,'description',NULL,NULL,'category/03.2518.00.jpg',0),(4,'Часы металлические',NULL,'Edelstein',0,0,'description',NULL,NULL,'category/NA.jpg',0),(5,'Часы \"Фристайл\"',NULL,'NA',0,0,'description',NULL,NULL,'category/03.2912.00.jpg',0),(10,'Летние мотивы',NULL,'Frischer Wind',1,0,'description',NULL,NULL,'category/NA.jpg',0),(11,'Дачный уголок',NULL,'Home & Garden',1,0,'Повесьте на стену часы и Вашей спальне и засыпайте в тишине! Кварцевый механизм абсолютно бесшумен и очень долговечен.',NULL,NULL,'category/NA.jpg',0),(12,'Милая кухонька',NULL,'Country-Style',1,0,'description',NULL,NULL,'category/NA.jpg',0),(13,'Загородный стиль',NULL,'Landhausstil',1,0,'Вы хотите украсить Ваш дачный домик или коттедж? Часы этой серии также окажутся как нельзя кстати, они идеально подходят для уютной комнаты загородного дома.',NULL,NULL,'category/NA.jpg',0),(14,'Маленькие',NULL,'Uhren klein',2,0,'description',NULL,NULL,'category/NA.jpg',0),(15,'Большие',NULL,'Uhren groß',2,0,'description',NULL,NULL,'category/NA.jpg',0);
/*!40000 ALTER TABLE `SS_categories` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SS_new_offers`
--

DROP TABLE IF EXISTS `SS_new_offers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_new_offers` (
  `offerID` int(11) NOT NULL AUTO_INCREMENT,
  `productID` int(11) DEFAULT NULL,
  `sort_order` int(11) DEFAULT NULL,
  PRIMARY KEY (`offerID`)
) ENGINE=MyISAM AUTO_INCREMENT=19 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_new_offers`
--

LOCK TABLES `SS_new_offers` WRITE;
/*!40000 ALTER TABLE `SS_new_offers` DISABLE KEYS */;
INSERT INTO `SS_new_offers` VALUES (18,10,1);
/*!40000 ALTER TABLE `SS_new_offers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SS_ordered_carts`
--

DROP TABLE IF EXISTS `SS_ordered_carts`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_ordered_carts` (
  `productID` int(11) NOT NULL,
  `orderID` int(11) NOT NULL,
  `name` char(255) DEFAULT NULL,
  `Price` float DEFAULT NULL,
  `Quantity` int(11) DEFAULT NULL,
  PRIMARY KEY (`productID`,`orderID`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_ordered_carts`
--

LOCK TABLES `SS_ordered_carts` WRITE;
/*!40000 ALTER TABLE `SS_ordered_carts` DISABLE KEYS */;
/*!40000 ALTER TABLE `SS_ordered_carts` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SS_orders`
--

DROP TABLE IF EXISTS `SS_orders`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_orders` (
  `orderID` int(11) NOT NULL AUTO_INCREMENT,
  `order_time` datetime DEFAULT NULL,
  `cust_firstname` varchar(30) DEFAULT NULL,
  `cust_lastname` varchar(30) DEFAULT NULL,
  `cust_email` varchar(30) DEFAULT NULL,
  `cust_country` varchar(30) DEFAULT NULL,
  `cust_zip` varchar(30) DEFAULT NULL,
  `cust_state` varchar(30) DEFAULT NULL,
  `cust_city` varchar(30) DEFAULT NULL,
  `cust_address` varchar(30) DEFAULT NULL,
  `cust_phone` varchar(30) DEFAULT NULL,
  PRIMARY KEY (`orderID`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_orders`
--

LOCK TABLES `SS_orders` WRITE;
/*!40000 ALTER TABLE `SS_orders` DISABLE KEYS */;
/*!40000 ALTER TABLE `SS_orders` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SS_products`
--

DROP TABLE IF EXISTS `SS_products`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_products` (
  `productID` int(11) NOT NULL AUTO_INCREMENT,
  `categoryID` int(11) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  `name_en` varchar(255) DEFAULT NULL,
  `name_de` varchar(255) DEFAULT NULL,
  `description` text,
  `description_en` text,
  `description_de` text,
  `customers_rating` float NOT NULL,
  `Price` float DEFAULT NULL,
  `picture` varchar(255) DEFAULT NULL,
  `in_stock` int(11) DEFAULT NULL,
  `thumbnail` varchar(255) DEFAULT NULL,
  `customer_votes` int(11) NOT NULL,
  `items_sold` int(11) NOT NULL,
  `big_picture` varchar(255) DEFAULT NULL,
  `enabled` int(11) NOT NULL,
  `brief_description` text,
  `brief_description_en` text,
  `brief_description_de` text,
  `list_price` float DEFAULT NULL,
  `stock_price` float DEFAULT NULL,
  `product_code` char(25) DEFAULT NULL,
  PRIMARY KEY (`productID`)
) ENGINE=MyISAM AUTO_INCREMENT=58 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_products`
--

LOCK TABLES `SS_products` WRITE;
/*!40000 ALTER TABLE `SS_products` DISABLE KEYS */;
INSERT INTO `SS_products` VALUES (1,10,'Пчелиное лакомство',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 15 х 22 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2201.00.jpg',1,'thumbnail/00.2201.00.jpg',0,0,'big/00.2201.00.jpg',1,'Размер: 15 х 22 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2201.00'),(2,10,'Буренка',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2204.00.jpg',1,'thumbnail/00.2204.00.jpg',0,0,'big/00.2204.00.jpg',1,'Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2204.00'),(3,10,'Мишка с горшочком меда',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 21 х 30 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2212.00.jpg',1,'thumbnail/00.2212.00.jpg',0,0,'big/00.2212.00.jpg',1,'Размер: 21 х 30 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2212.00'),(4,10,'Лимончик',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 19 х 26 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2213.00.jpg',1,'thumbnail/00.2213.00.jpg',0,0,'big/00.2213.00.jpg',1,'Размер: 19 х 26 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2213.00'),(5,10,'Лягушка-квакушка',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 25 х 26 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2214.00.jpg',1,'thumbnail/00.2214.00.jpg',0,0,'big/00.2214.00.jpg',1,'Размер: 25 х 26 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2214.00'),(6,10,'Филин и ежик',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 19 x 27см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2217.00.jpg',1,'thumbnail/00.2217.00.jpg',0,0,'big/00.2217.00.jpg',1,'Размер: 19 x 27см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2217.00'),(7,10,'Чаепитие',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 18 х 25 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2225.00.jpg',1,'thumbnail/00.2225.00.jpg',0,0,'big/00.2225.00.jpg',1,'Размер: 18 х 25 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2225.00'),(8,10,'Мышка на сыре',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 21 х 18 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2227.00.jpg',1,'thumbnail/00.2227.00.jpg',0,0,'big/00.2227.00.jpg',1,'Размер: 21 х 18 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2227.00'),(9,10,'Веселая пчелка',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 18 х 25 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2233.00.jpg',1,'thumbnail/00.2233.00.jpg',0,0,'big/00.2233.00.jpg',1,'Размер: 18 х 25 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2233.00'),(10,10,'Ромашка ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 21 х 18 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2221.00.jpg',1,'thumbnail/00.2221.00.jpg',0,0,'big/00.2221.00.jpg',1,'Размер: 21 х 18 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2221.00'),(11,10,'Собачья радость (далматинец)     ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: белый. Размер: 20 х 25 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2231.00.jpg',1,'thumbnail/00.2231.00.jpg',0,0,'big/00.2231.00.jpg',1,'Размер: 20 х 25 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2231.00'),(12,10,'Собачья радость ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 20 х 25 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.2231.00.jpg',1,'thumbnail/03.2231.00.jpg',0,0,'big/03.2231.00.jpg',1,'Размер: 20 х 25 см. (размер без маятника)',NULL,NULL,2199,26.85,'03.2231.00'),(13,10,'Тосты с джемом  ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 18 х 23 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2206.00.jpg',1,'thumbnail/00.2206.00.jpg',0,0,'big/00.2206.00.jpg',1,'Размер: 18 х 23 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2206.00'),(14,10,'Цветная капуста',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 25 х 21 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2220.00.jpg',1,'thumbnail/00.2220.00.jpg',0,0,'big/00.2220.00.jpg',1,'Размер: 25 х 21 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2220.00'),(15,10,'Баночка варенья',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 20 х 21 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2226.00.jpg',1,'thumbnail/00.2226.00.jpg',0,0,'big/00.2226.00.jpg',1,'Размер: 20 х 21 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2226.00'),(16,10,'Подсолнухи в горшочке',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 20 х 28 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2228.00.jpg',1,'thumbnail/00.2228.00.jpg',0,0,'big/00.2228.00.jpg',1,'Размер: 20 х 28 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2228.00'),(17,11,'Корзинка для пикника ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 20 x 23 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.2307.00.jpg',1,'thumbnail/03.2307.00.jpg',0,0,'big/03.2307.00.jpg',1,'Размер: 20 x 23 см. (размер без маятника)',NULL,NULL,2199,26.85,'03.2307.00'),(18,11,'Корзинка для пикника ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 20 x 23 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2307.00.jpg',1,'thumbnail/00.2307.00.jpg',0,0,'big/00.2307.00.jpg',1,'Размер: 20 x 23 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2307.00'),(19,11,'Бодрое утро ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: с синей крышечкой. Размер: 22 х 24 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2302.00.jpg',1,'thumbnail/00.2302.00.jpg',0,0,'big/00.2302.00.jpg',1,'Размер: 22 х 24 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2302.00'),(20,11,'Горшочек с оливками',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 19 х 21 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.2306.00.jpg',1,'thumbnail/03.2306.00.jpg',0,0,'big/03.2306.00.jpg',1,'Размер: 19 х 21 см. (размер без маятника)',NULL,NULL,2199,26.85,'03.2306.00'),(21,11,'Огородная радость',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: белый. Размер: 21 х 24 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.2300.00.jpg',1,'thumbnail/03.2300.00.jpg',0,0,'big/03.2300.00.jpg',1,'Размер: 21 х 24 см. (размер без маятника)',NULL,NULL,2199,26.85,'03.2300.00'),(22,11,'Подарок садоводу',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 18 х 26 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2301.00.jpg',1,'thumbnail/00.2301.00.jpg',0,0,'big/00.2301.00.jpg',1,'Размер: 18 х 26 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2301.00'),(23,11,'Молочный бидончик ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: белый. Размер: 18 х 26 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.2303.00.jpg',1,'thumbnail/03.2303.00.jpg',0,0,'big/03.2303.00.jpg',1,'Размер: 18 х 26 см. (размер без маятника)',NULL,NULL,2199,26.85,'03.2303.00'),(24,11,'Весы с вишенками',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 19 х 27 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2304.00.jpg',1,'thumbnail/00.2304.00.jpg',0,0,'big/00.2304.00.jpg',1,'Размер: 19 х 27 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2304.00'),(25,11,'Чистюля      ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 21 х 23 см. (размер без маятника)',NULL,NULL,0,2199,'small/00.2305.00.jpg',1,'thumbnail/00.2305.00.jpg',0,0,'big/00.2305.00.jpg',1,'Размер: 21 х 23 см. (размер без маятника)',NULL,NULL,2199,26.85,'00.2305.00'),(26,12,'Кошкин дом ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2911.P0.jpg',1,'thumbnail/00.2911.P0.jpg',0,0,'big/00.2911.P0.jpg',1,'Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2911.P0'),(27,12,'Кошкин дом ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2911.P0.jpg',1,'thumbnail/03.2911.P0.jpg',0,0,'big/03.2911.P0.jpg',1,'Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2911.P0'),(28,12,'Кошкин дом ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: зеленый. Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,0,2999,'small/04.2911.P0.jpg',1,'thumbnail/04.2911.P0.jpg',0,0,'big/04.2911.P0.jpg',1,'Размер: 23 х 28 см. (размер без маятника)',NULL,NULL,2999,37.95,'04.2911.P0'),(29,12,'Полет на дельтаплане',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 22 х 30 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2950.P0.jpg',1,'thumbnail/03.2950.P0.jpg',0,0,'big/03.2950.P0.jpg',1,'Размер: 22 х 30 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2950.P0'),(30,12,'Брат-пилот',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 21 х 24 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2952.P0.jpg',1,'thumbnail/00.2952.P0.jpg',0,0,'big/00.2952.P0.jpg',1,'Размер: 21 х 24 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2952.P0'),(31,12,'Кухонька ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: белый. Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2906.P0.jpg',1,'thumbnail/03.2906.P0.jpg',0,0,'big/03.2906.P0.jpg',1,'Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2906.P0'),(32,12,'Кухонька ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: голубой. Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2906.P0.jpg',1,'thumbnail/00.2906.P0.jpg',0,0,'big/00.2906.P0.jpg',1,'Размер: 24 х 27 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2906.P0'),(33,12,'Старинная кофемолка ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 25 х 22 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2907.P0.jpg',1,'thumbnail/00.2907.P0.jpg',0,0,'big/00.2907.P0.jpg',1,'Размер: 25 х 22 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2907.P0'),(34,12,'Пряности',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 16 х 24 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2901.P0.jpg',1,'thumbnail/03.2901.P0.jpg',0,0,'big/03.2901.P0.jpg',1,'Размер: 16 х 24 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2901.P0'),(35,12,'Легкий завтрак ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 18 х 22 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2902.P0.jpg',1,'thumbnail/03.2902.P0.jpg',0,0,'big/03.2902.P0.jpg',1,'Размер: 18 х 22 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2902.P0'),(36,12,'Легкий завтрак ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 18 х 22 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2902.P0.jpg',1,'thumbnail/00.2902.P0.jpg',0,0,'big/00.2902.P0.jpg',1,'Размер: 18 х 22 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2902.P0'),(37,12,'Затеваем пироги',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 25 х 26 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2904.P0.jpg',1,'thumbnail/03.2904.P0.jpg',0,0,'big/03.2904.P0.jpg',1,'Размер: 25 х 26 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2904.P0'),(38,12,'Доброе утро ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 27 х 20 см. (размер без маятника)',NULL,NULL,0,2999,'small/03.2903.P0.jpg',1,'thumbnail/03.2903.P0.jpg',0,0,'big/03.2903.P0.jpg',1,'Размер: 27 х 20 см. (размер без маятника)',NULL,NULL,2999,37.95,'03.2903.P0'),(39,12,'Доброе утро ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 27 х 20 см. (размер без маятника)',NULL,NULL,0,2999,'small/00.2903.P0.jpg',1,'thumbnail/00.2903.P0.jpg',0,0,'big/00.2903.P0.jpg',1,'Размер: 27 х 20 см. (размер без маятника)',NULL,NULL,2999,37.95,'00.2903.P0'),(40,13,'Чайкоффский',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 16 х 15 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.1104.P0.jpg',1,'thumbnail/03.1104.P0.jpg',0,0,'big/03.1104.P0.jpg',1,'Размер: 16 х 15 см. (размер без маятника)',NULL,NULL,2199,26.75,'03.1104.P0'),(41,13,'Конфитюр',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14 х 14 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.1105.P0.jpg',1,'thumbnail/03.1105.P0.jpg',0,0,'big/03.1105.P0.jpg',1,'Размер: 14 х 14 см. (размер без маятника)',NULL,NULL,2199,26.75,'03.1105.P0'),(42,13,'Тосты с медом',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 15 х 16 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.1106.P0.jpg',1,'thumbnail/03.1106.P0.jpg',0,0,'big/03.1106.P0.jpg',1,'Размер: 15 х 16 см. (размер без маятника)',NULL,NULL,2199,26.75,'03.1106.P0'),(43,13,'Кофе со сливками',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 17 х 15 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.1101.P0.jpg',1,'thumbnail/03.1101.P0.jpg',0,0,'big/03.1101.P0.jpg',1,'Размер: 17 х 15 см. (размер без маятника)',NULL,NULL,2199,26.75,'03.1101.P0'),(44,13,'Фруктовое настроение    ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14 х 19 см. (размер без маятника)',NULL,NULL,0,2199,'small/03.1102.P0.jpg',1,'thumbnail/03.1102.P0.jpg',0,0,'big/03.1102.P0.jpg',1,'Размер: 14 х 19 см. (размер без маятника)',NULL,NULL,2199,26.75,'03.1102.P0'),(45,14,'Птичий двор',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14,5 x 20 см. ',NULL,NULL,0,2399,'small/03.2524.00.jpg',1,'thumbnail/03.2524.00.jpg',0,0,'big/03.2524.00.jpg',1,'Размер: 14,5 x 20 см. ',NULL,NULL,2399,28.85,'03.2524.00'),(46,14,'Натюрморт',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14,5 x 20 см. ',NULL,NULL,0,2399,'small/03.2531.00.jpg',1,'thumbnail/03.2531.00.jpg',0,0,'big/03.2531.00.jpg',1,'Размер: 14,5 x 20 см. ',NULL,NULL,2399,28.85,'03.2531.00'),(47,14,'В стиле \"Кантри\"',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14,5 x 20 см. ',NULL,NULL,0,2399,'small/03.2534.00.jpg',1,'thumbnail/03.2534.00.jpg',0,0,'big/03.2534.00.jpg',1,'Размер: 14,5 x 20 см. ',NULL,NULL,2399,28.85,'03.2534.00'),(48,14,'Гуси с сердечком',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14,5 x 20 см. ',NULL,NULL,0,2399,'small/03.2556.00.jpg',1,'thumbnail/03.2556.00.jpg',0,0,'big/03.2556.00.jpg',1,'Размер: 14,5 x 20 см. ',NULL,NULL,2399,28.85,'03.2556.00'),(49,14,'Букет для мамы',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Размер: 14,5 x 20 см. ',NULL,NULL,0,2399,'small/03.2518.00.jpg',1,'thumbnail/03.2518.00.jpg',0,0,'big/03.2518.00.jpg',1,'Размер: 14,5 x 20 см. ',NULL,NULL,2399,28.85,'03.2518.00'),(50,15,'Доброе тепло',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет:  . Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/00.2623.00.jpg',1,'thumbnail/00.2623.00.jpg',0,0,'big/00.2623.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'00.2623.00'),(51,15,'Урожай ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет:             . Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/00.2622.00.jpg',1,'thumbnail/00.2622.00.jpg',0,0,'big/00.2622.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'00.2622.00'),(52,15,'Летний день ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: красный . Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/03.2634.00.jpg',1,'thumbnail/03.2634.00.jpg',0,0,'big/03.2634.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'03.2634.00'),(53,15,'Отдохнем!  ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/03.2637.00.jpg',1,'thumbnail/03.2637.00.jpg',0,0,'big/03.2637.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'03.2637.00'),(54,15,'Гусиная пара',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/03.2644.00.jpg',1,'thumbnail/03.2644.00.jpg',0,0,'big/03.2644.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'03.2644.00'),(55,15,'Специи',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: синий. Размер: 21 x 30 см. ',NULL,NULL,0,3199,'small/00.2641.00.jpg',1,'thumbnail/00.2641.00.jpg',0,0,'big/00.2641.00.jpg',1,'Размер: 21 x 30 см. ',NULL,NULL,3199,39.75,'00.2641.00'),(56,2,'Коровки на лугу',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: коричневый. Размер: 16,5 x 24,5 см. ',NULL,NULL,0,2299,'small/03.0971.00.jpg',1,'thumbnail/03.0971.00.jpg',0,0,'big/03.0971.00.jpg',1,'Размер: 16,5 x 24,5 см. ',NULL,NULL,2299,27.95,'03.0971.00'),(57,2,'Хозяюшка    ',NULL,NULL,'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. Цвет: корич. Размер: 16,5 x 24,5 см. ',NULL,NULL,0,2299,'small/03.0973.00.jpg',1,'thumbnail/03.0973.00.jpg',0,0,'big/03.0973.00.jpg',1,'Размер: 16,5 x 24,5 см. ',NULL,NULL,2299,27.95,'03.0973.00');
/*!40000 ALTER TABLE `SS_products` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SS_special_offers`
--

DROP TABLE IF EXISTS `SS_special_offers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SS_special_offers` (
  `offerID` int(11) NOT NULL AUTO_INCREMENT,
  `productID` int(11) DEFAULT NULL,
  `sort_order` int(11) DEFAULT NULL,
  PRIMARY KEY (`offerID`)
) ENGINE=MyISAM AUTO_INCREMENT=21 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SS_special_offers`
--

LOCK TABLES `SS_special_offers` WRITE;
/*!40000 ALTER TABLE `SS_special_offers` DISABLE KEYS */;
INSERT INTO `SS_special_offers` VALUES (20,26,1),(3,3,NULL),(18,1,1),(17,33,1),(19,23,1),(14,24,1);
/*!40000 ALTER TABLE `SS_special_offers` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-02-20 11:40:40
