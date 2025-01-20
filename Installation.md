# French
Pour les utisateurs francophones, la procédure d'installation de easieR est décrite ici :
https://theeasierproject.wordpress.com/2017/05/05/comment-installer-easier/

# English

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



Finally, easieR requires pandoc. For mac users, you can download and install pandoc here :
 <a href="https://github.com/jgm/pandoc/releases/tag/2.2.3.2" target="_blank" rel="noopener noreferrer">https://github.com/jgm/pandoc/releases/tag/2.2.3.2</a>


For windows users, pandoc is installed automatically when you are using the <code>easieR()</code> function. 
If it is not the case, please use the same procedure as mac users. 
