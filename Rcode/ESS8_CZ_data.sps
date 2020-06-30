* Encoding: UTF-8.
* ESS8 CZ data export from SAV to CSV.

GET FILE='C:\Users\djhurio\Dropbox\Darbs\ESS-SWEP\ESS-git\data\ESS8CZ\ESS8CZ.sav'.

SAVE TRANSLATE OUTFILE='C:\Users\djhurio\Dropbox\Darbs\ESS-SWEP\ESS-git\data\ESS8CZ\ESS8CZ.csv'
  /TYPE=CSV
  /ENCODING='UTF8'
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.
