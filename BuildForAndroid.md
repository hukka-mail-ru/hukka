<pre>

1. Установить necessistas (т.е. Android SDK и Qt-библиотеки, собранные для Android)<br>
2. Собрать либу из qt-исходников (с помощью Makefile) и положить ее в ../android/libs/armeabi<br>
<br>
3. Собрать java-приложение:   ~/distr/qt_android/ant-1.8.0/bin/ant debug<br>
4. Установить java-приложение на трубу:  adb install -r bin/Client-debug.apk<br>
<br>
5. Установить Qt-библиотеки на трубу, в каталог ministro<br>
<br>
</pre>