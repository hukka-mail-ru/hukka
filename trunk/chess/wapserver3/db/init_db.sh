#!/bin/sh
mysql -u root -p < from_root.sql
mysql -u WapServer3 -p < from_user.sql
