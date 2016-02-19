<h3>Общие сведения</h3>
EJB - это технология в составе J2EE.
Tutorial: http://docs.oracle.com/javaee/6/tutorial/doc/gexaj.html#gexba


<h3>Установка IDE</h3>

Рекомендуется установить Netbeans EE, приспособленный для работы с сервером glassfish. Сервер входит в поставку Netbeans EE.

Отдельный хелп по серверу glassfish:<br>
<a href='http://glassfish.java.net/downloads/quickstart/index.html'>http://glassfish.java.net/downloads/quickstart/index.html</a>

Также нужен ant, версии от 1.7<br>
<br>
<h3>glassfish </h3>
Войти на сервер: ~/distr/glassfish-3.1.1/bin/asadmin и выполнить:<br>
<blockquote>start-domain<br>
start-database --dbhome /home/ssy/distr/glassfish3/javadb<br>
<br>
После этого становится доступен вход на сервер через web-interface:<br>
<a href='http://localhost:4848'>http://localhost:4848</a>
<br>
развернуть приложение: deploy sample-dir/hello.war<br>
обратиться к веб-интерфейсу приложения: <a href='http://localhost:8080/hello'>http://localhost:8080/hello</a><br>
список развернутых приложений: list-applications<br>
<br></blockquote>


<h3>Исходники примеров </h3>

You can get the examples using the Update Tool.<br>
A detailed explanation can be found in the tutorial itself.<br>
<br>
В настройках Update Tool-а надо включить ВСЕ источники обновления!<br>
(источник с Java tutotial-ом по умолчанию выключен)<br>
<br>
<br>
<h3>Сборка примеров из tutorial-a</h3>
<pre>
cd ~/distr<br>
ln -s glassfish3/glassfish/docs/javaee-tutorial<br>
cd ~/distr/javaee-tutorial/examples/ejb/converter<br>
ant<br>
</pre>
Ant создает каталог build.<br>
<br>
<br>
<h3>Hello World: J2EE application client</h3>

<a href='http://java.sun.com/j2ee/tutorial/1_3-fcs/doc/GettingStarted.html'>http://java.sun.com/j2ee/tutorial/1_3-fcs/doc/GettingStarted.html</a>
<br>
<br>
<a href='http://java.sun.com/j2ee/sdk_1.3/install.html'>http://java.sun.com/j2ee/sdk_1.3/install.html</a>
<a href='http://java.sun.com/j2ee/tutorial/1_3-fcs/doc/GettingStarted2.html#67955'>http://java.sun.com/j2ee/tutorial/1_3-fcs/doc/GettingStarted2.html#67955</a>