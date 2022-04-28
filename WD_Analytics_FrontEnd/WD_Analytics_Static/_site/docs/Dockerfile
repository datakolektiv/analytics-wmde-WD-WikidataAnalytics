### ---------------------------------------------------------------------------
### --- Wikidata Analytics Portal
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Analytics (WA)
### --- https://wikidata-analytics.wmflabs.org/
### ---
### --- WA is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WA is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WA If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

FROM openanalytics/r-base

# install required R packages
RUN install.r servr

# install pandoc
RUN wget https://github.com/jgm/pandoc/releases/download/2.6/pandoc-2.6-1-amd64.deb
RUN dpkg -i pandoc-2.6-1-amd64.deb

COPY docs /root/docs/

# use default port for shinyproxy apps, so you don't have to add port in application.yml
EXPOSE 3838  

CMD ["R", "-e", "servr::httd('/root/docs', port = 3838, host = '0.0.0.0')"]