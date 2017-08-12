# Plan for moving forward

So it seems as if the MediaWiki import might be a bit more challenging than I anticipated.
The export XML of the main page looks like this: [this](https://github.com/dtekcth/wikipageconverter/blob/feature/mediawiki-backend/Data/WikiPageConverter/MediaWiki/Huvudsida.xml).

My plan is to create some kind of script that creates a bunch of files, one for an in-memory user database so I can map 
the authors of the page revisions to actual MW users and one in-memory namespace database, so we don't need to modify the source
code of the import program if we update the namespaces.
Moreover we need to get the next available PageId from the database, as the MySQL database uses auto_increment on the page ids, 
and then store this information in a State so we can update it every time we add a new page. We could also store the RevisionId 
in the State since that is also an auto-incrementing reference.

So that is my plan, retrieve a bunch of information from the DB, keep it in some kind of State monad / StateT monad stack and 
then **simply** convert the revisions and everything else to XML.
