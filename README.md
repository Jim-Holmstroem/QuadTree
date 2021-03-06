Start out
---------
* Install requirements, see requirements.txt
* A simple demo with visualisations of the datastructure and algorithm ``ghc --make Demo.hs && ./Demo 8192 0.5 0.3 0.1``
* Read instructions below on how to get the database. Then start the world demo by ``ghc --make WorldDemo.hs && ./WorldDemo``

![Rendering of the algorithm](https://raw.githubusercontent.com/Jim-Holmstroem/QuadTree/master/example_output/random.8192.debug.png)

Note
----
The world demo is according to a "flat world distance" i.e. only comparing the longitudes and latitudes without considering the curvature and elipticity of the earth and such. It will be an accurate measure locally but not over large distances etc.
This is okey since it's only used as an example of real-world distribution of points (compared to the ones generated by a random variable (uniform or similar)).


City Database
-------------
Should be downloaded manually from [https://www.maxmind.com/en/free-world-cities-database](https://www.maxmind.com/en/free-world-cities-database)

And convert to UTF-8 with
```bash
iconv -f ISO-8859-1 -t UTF-8 worldcitiespop.txt -o worldcitiespop.utf8.txt
```
