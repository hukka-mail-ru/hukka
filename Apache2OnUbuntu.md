1. sudo aptitude install apache2 <br>
<blockquote>sudo aptitude install php5<br>
sudo aptitude install php5-mysql<br></blockquote>

2. In browser: <a href='http://127.0.0.1/'>http://127.0.0.1/</a>   => it works! <br>
3. sudo gedit /etc/apache2/sites-available/default => set my directory<br>
4. sudo gedit /etc/apache2/httpd.conf => add:  ServerName localhost<br>
5. sudo /etc/init.d/apache2 restart  <br>


Mysql collation settings: <a href='http://cameronyule.com/2008/07/configuring-mysql-to-use-utf-8'>http://cameronyule.com/2008/07/configuring-mysql-to-use-utf-8</a>