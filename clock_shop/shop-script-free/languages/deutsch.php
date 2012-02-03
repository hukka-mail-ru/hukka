<?php
/*****************************************************************************
 *                                                                           *
 * Shop-Script FREE                                                          *
 * Copyright (c) 2006 Articus consulting group. All rights reserved.         *
 *                                                                           *
 ****************************************************************************/
 
 
 
//language file

//		RUSSIAN		//

//default character set, that will be used
define('DEFAULT_CHARSET', 'utf-8');
define('LINK_TO_HOMEPAGE', 'Startseite');
define('PRODUCTS_BEST_CHOISE', '<h5>Beste Produkte</h5>');
define('MORE_INFO_ON_PRODUCT', 'Beschreibung..');
define('ENLARGE_PICTURE', 'vergrößern...');
define('ADD_TO_CART_STRING', 'in den Korb');
define('LIST_PRICE', 'Alter Preis');
define('CURRENT_PRICE', 'Unser Preis');
define('YOU_SAVE', 'Sie sparen');
define('IN_STOCK', 'Auf Lager');
define('VOTING_FOR_ITEM_TITLE', 'Stimmen Sie für die Ware!');
define('MARK_EXCELLENT', 'Super!');
define('MARK_GOOD', 'Gut');
define('MARK_AVERAGE', 'Mäßig');
define('MARK_POOR', 'Schlecht');
define('MARK_PUNY', 'Sehr schlecht');
define('VOTE_BUTTON', 'Stimmen!');
define('VOTES_FOR_ITEM_STRING', 'Stimmen');

define('LOGOUT_LINK', 'Выйти из сеанса...');
define('ADMINISTRATE_LINK', '>> АДМИНИСТРИРОВАНИЕ <<');

define('ANSWER_YES', 'ja');
define('ANSWER_NO', 'nein');
define('SAVE_BUTTON', 'Speichern');
define('DELETE_BUTTON', 'Entfernen');
define('CLOSE_BUTTON', 'Schließen');
define('CANCEL_BUTTON', 'Abbrechen');
define('UPDATE_BUTTON', 'Aktualisieren');
define('ADD_BUTTON', 'Hinzufügen');
define('ADMIN_ENABLED', 'Einschalten');

define('STRING_BACK_TO_SHOPPING', 'Zurück...');
define('STRING_SHOW', 'anzeigen');
define('STRING_NUMBER', 'Zahlen');
define('STRING_RELATED_ITEMS', 'Wir raten auch an');
define('STRING_NUMBER_ONLY', 'nur Zahl');
define('STRING_EMPTY_CATEGORY', 'keine Waren');
define('STRING_NO_ORDERS', 'keine Bestellungen');
define('STRING_SEARCH', 'Suche');
define('STRING_LANGUAGE', 'Sprache');
define('STRING_PRICELIST', 'Preisliste');
define('STRING_GREETINGS', '');
define('STRING_FOUND', 'Ergebnisse der Suche');
define('STRING_NO_MATCHES_FOUND', 'keine Ergebnisse');
define('STRING_PRODUCTS', 'Ware(n)');
define('STRING_ORDER_ID', 'Bestellungnummer');
define('STRING_ORDER_PLACED', '<div align="center"><h1>Vielen dank für Ihre Bestellung!</h1><h3>Bald werden wir Sie für Bestätigung der Bestellung kontaktiren</h3></div>');
define('STRING_PLACE_ORDER', 'Bestellen!');
define('STRING_NEXT', 'nächste');
define('STRING_PREVIOUS', 'vorig');
define('STRING_SHOWALL', 'zeige alle');
define('STRING_REQUIRED', '<font color=red>*</font> füllen Sie bitte unbedingt');
define('STRING_CONTACT_INFORMATION', 'KONTAKT INFORMATION');

define('CART_CONTENT_EMPTY', '(keine Ware)');
define('CART_CONTENT_NOT_EMPTY', 'Ware(n): ');
define('CART_TITLE', 'Warenkorb');
define('CART_CLEAR', 'löschen');
define('CART_PROCEED_TO_CHECKOUT', 'Bestellen');
define('CART_EMPTY', 'Ihr Warenkorb ist derzeit leer');

//table titles

define('TABLE_PRODUCT_NAME', 'Bezeichnung');
define('TABLE_PRODUCT_QUANTITY', 'Menge');
define('TABLE_PRODUCT_COST', 'Preis');
define('TABLE_TOTAL', 'Summe:');
define('TABLE_ORDER_TIME', 'Bestellungzeit');
define('TABLE_ORDERED_PRODUCTS', 'Bestellte Ware');
define('TABLE_ORDER_TOTAL', 'Kosten der Bestellung');
define('TABLE_CUSTOMER', 'Käufer');

//different admin strings

define('ADMIN_TITLE', 'Администрирование');

define('ADMIN_WELCOME', '<p><font class=big>Добро пожаловать в режим администрирования!</font><p>Используйте меню для доступа к разделам администраторской части.');
define('ADMIN_NEW_ORDERS', 'Новые заказы');
define('ADMIN_CATEGORIES_PRODUCTS', 'Категории и товары');
define('ADMIN_CATALOG', 'Katalog');
define('ADMIN_SETTINGS', 'Einstellung');
define('ADMIN_SETTINGS_GENERAL', 'Общие');
define('ADMIN_SETTINGS_APPEARENCE', 'Оформление');
define('ADMIN_CUSTOMERS_AND_ORDERS', 'Заказы');
define('ADMIN_ABOUT_PAGE', 'Über uns');
define('ADMIN_SHIPPING_PAGE', 'Versandkosten und Bezahlung');
define('ADMIN_AUX_INFO', 'Дополнительная информация');
define('ADMIN_BACK_TO_SHOP', 'в общедоступную часть ...');
define('ADMIN_SORT_ORDER', 'Порядок сортировки');
define('ADMIN_LOGIN_PASSWORD', 'Доступ к администрированию');
define('ADMIN_CHANGE_LOGINPASSWORD', 'Изменить логин и пароль администратора');
define('ADMIN_CURRENT_LOGIN', 'Логин');
define('ADMIN_OLD_PASS', 'Старый пароль');
define('ADMIN_NEW_PASS', 'Новый пароль');
define('ADMIN_NEW_PASS_CONFIRM', 'Подтвердите новый пароль');
define('ADMIN_UPDATE_SUCCESSFUL', '<font color=blue><b>Обновление прошло успешно!</b></font>');
define('ADMIN_NO_SPECIAL_OFFERS', 'Спец-предложения не выбраны');
define('ADMIN_ADD_SPECIAL_OFFERS', 'Добавить в список спец-предложений');
define('ADMIN_SPECIAL_OFFERS_DESC', 'Спец-предложения показываются на витрине Вашего магазина.<br>
Выбрать товарные позиции, которые будут показаны как спец-предложения<br>
Вы можете в подразделе <a href="admin.php?dpt=catalog&sub=products_categories">"Категории и товары"</a>, кликнув по значку <img src="images/admin_special_offer.gif" border=0> в таблице товаров.<br>
В спец-предложения можно выбрать только товары с фотографией.');
define('ADMIN_ROOT_WARNING', '<font color=red>Все товары, находящиеся в корне, не видны пользователям!</font>');
define('ADMIN_ABOUT_PRICES', '<font class=small>// цены актуальны на момент заказа и указаны без налога //</font>');
define('ADMIN_SHOP_NAME', 'Название магазина');
define('ADMIN_SHOP_URL', 'URL магазина');
define('ADMIN_SHOP_EMAIL', 'Контактный email адрес Вашего магазина');
define('ADMIN_ORDERS_EMAIL', 'Email, на который будут отправляться уведомления о заказах');
define('ADMIN_SHOW_ADD2CART', 'Включить возможность добавления товаров в корзину и оформления заказов');
define('ADMIN_SHOW_BESTCHOICE', 'Показывать наиболее популярные товары в пустых категориях');
define('ADMIN_MAX_PRODUCTS_COUNT_PER_PAGE', 'Максимальное количество товаров на странице');
define('ADMIN_MAX_COLUMNS_PER_PAGE', 'Количество столбцов при показе товаров');
define('ADMIN_MAIN_COLORS', 'Цвета, используемые для отображения таблиц:');
define('ADMIN_COLOR', 'Цвет');
define('ADMIN_SPECIAL_OFFERS', 'Spezialangebote');
define('ADMIN_CATEGORY_TITLE', 'Категории');
define('ADMIN_CATEGORY_NEW', 'Создать новую категорию');
define('ADMIN_CATEGORY_PARENT', 'Родительская категория:');
define('ADMIN_CATEGORY_MOVE_TO', 'Переместить в:');
define('ADMIN_CATEGORY_NAME', 'Название категории:');
define('ADMIN_CATEGORY_LOGO', 'Логотип:');
define('ADMIN_CATEGORY_ROOT', 'Корень');
define('ADMIN_CATEGORY_DESC', 'Описание');
define('ADMIN_PRODUCT_TITLE', 'Товары');
define('ADMIN_PRODUCT_NEW', 'Добавить новый товар');
define('ADMIN_PRODUCT_CODE', 'Внутренний код (артикул)');
define('ADMIN_PRODUCT_NAME', 'Наименование');
define('ADMIN_PRODUCT_RATING', 'Рейтинг');
define('ADMIN_PRODUCT_PRICE', 'Цена');
define('ADMIN_PRODUCT_LISTPRICE', 'Старая цена');
define('ADMIN_PRODUCT_INSTOCK', 'На складе');
define('ADMIN_PRODUCT_PICTURE', 'Фотография');
define('ADMIN_PRODUCT_THUMBNAIL', 'Маленькая фотография');
define('ADMIN_PRODUCT_BIGPICTURE', 'Большая фотография');
define('ADMIN_PRODUCT_DESC', 'Описание');
define('ADMIN_PRODUCT_BRIEF_DESC', 'Краткое описание');
define('ADMIN_PRODUCT_SOLD', 'Продано');
define('CUSTOMER_EMAIL', 'Email:');
define('CUSTOMER_FIRST_NAME', 'Name:');
define('CUSTOMER_LAST_NAME', 'Nachname:');
define('CUSTOMER_ZIP', 'Postleitzahl:');
define('CUSTOMER_STATE', 'Region:');
define('CUSTOMER_COUNTRY', 'Land:');
define('CUSTOMER_CITY', 'Stadt:');
define('CUSTOMER_ADDRESS', 'Adresse:');
define('CUSTOMER_PHONE_NUMBER', 'Telefon:');

define('ADMIN_PICTURE_NOT_UPLOADED', '(фотография не загружена)');


//errors

define('ERROR_FAILED_TO_UPLOAD_FILE', '<b><font color=red>Не удалось закачать файл на сервер. Убедитесь,<br>что включены права на создание файлов на сервере в папке products_pictures/</font></b>');
define('ERROR_CANT_FIND_REQUIRED_PAGE', 'Извините, запрашиваемый документ не был найден на сервере');
define('ERROR_INPUT_EMAIL', 'Schreiben Sie bitte Ihren Email ein');
define('ERROR_INPUT_NAME', 'Schreiben Sie bitte Ihren Namen ein');
define('ERROR_INPUT_COUNTRY', 'Schreiben Sie bitte Ihres Land ein');
define('ERROR_INPUT_CITY', 'Schreiben Sie bitte Ihre Stadt ein');
define('ERROR_INPUT_ZIP', 'Schreiben Sie bitte Ihre Postzahl ein');
define('ERROR_INPUT_STATE', 'Schreiben Sie bitte Ihre Region ein');
define('ERROR_FILL_FORM', 'Schreiben Sie bitte aller Felde ein');
define('ERROR_WRONG_PASSWORD', 'Ihr altes Kennwort ist ungultig');
define('ERROR_PASS_CONFIRMATION', 'Kennworte sind nicht gleich');

//questions

define('QUESTION_DELETE_PICTURE', 'Удалить фотографию?');
define('QUESTION_DELETE_CONFIRMATION', 'Удалить?');

//emails
define('EMAIL_ADMIN_ORDER_NOTIFICATION_SUBJECT', 'Новый заказ!');
define('EMAIL_CUSTOMER_ORDER_NOTIFICATION_SUBJECT', 'Ваш заказ');
define('EMAIL_MESSAGE_PARAMETERS', 'Content-Type: text/plain; charset="'.DEFAULT_CHARSET.'"');
define('EMAIL_HELLO', 'Здравствуйте');
define('EMAIL_SINCERELY', 'С уважением');
define('EMAIL_THANK_YOU_FOR_SHOPPING_AT', 'Спасибо за Ваш выбор');
define('EMAIL_ORDER_WILL_BE_SHIPPED_TO', 'Ваш заказ будет доставлен по адресу');
define('EMAIL_OUR_MANAGER_WILL_CONTACT_YOU', 'Мы свяжемся с Вами для подтверждения заказа в ближайшее время.');

//warnings

define('WARNING_DELETE_INSTALL_PHP', 'Файл <b>install.php</b> не удален из директории со скриптами Shop-Script. Вам необходимо удалить его вручную.<br>');
define('WARNING_DELETE_FORGOTPW_PHP', 'Файл <b>forgot_password.php</b> не удален из директории со скриптами Shop-Script. Вам необходимо удалить его вручную.<br>');
define('WARNING_WRONG_CHMOD', 'Неверные атрибуты для папки cfg, ее содержимого, а также папок products_pictures и templates_c (либо эти не из этих папок существуют).<br>Необходимо установить правильные атрибуты на них для разрешения (пере)записи файлов в этих папках (как правило, это атрибуты 775).');

//currencies

define('ADMIN_CURRENCY', 'Валюта');
define('ADMIN_CURRENCY_ID_LEFT', 'Обозначение валюты слева от суммы (цены)<br>(например, "$")');
define('ADMIN_CURRENCY_ID_RIGHT', 'Обозначение валюты справа от суммы (цены)<br>(например, "руб.")');
define('ADMIN_CURRENCY_ISO3', 'Трехбуквенный код валюты ISO3<br>(например, USD, EUR, RUR)');

?>
