For the installation of easieR, you must :

1) install the 'devtools' package and load it 
<pre class="prettyprint lang-r">
install.packages("devtools")
library("devtools")
</pre>

2) install easieR
<pre class="prettyprint lang-r">
install_github("NicolasStefaniak/easieR")
</pre>

3) Given that easieR also uses the WRS package, you must install it 
(see https://github.com/nicebread/WRS/blob/master/README.md in case of trouble)
<pre class="prettyprint lang-r">
install_github("nicebread/WRS", subdir="pkg")
</pre>

Finally, easieR requires pandoc. For mac users, you can download and install pandoc here :
 <a href="https://github.com/jgm/pandoc/releases/tag/2.2.3.2" target="_blank" rel="noopener noreferrer">https://github.com/jgm/pandoc/releases/tag/2.2.3.2</a>


For windows users, pandoc is installed automatically when you are using the <code>easieR()</code> function. 
If it is not the case, please use the same procedure as mac users. 
