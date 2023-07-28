import sqlite3
import pandas as pd
from time import time

def convert_to_sql(package_manager):
    # load data
    df = pd.read_csv(f'../DockerCleaner/package-versions/{package_manager}.csv')

    # strip whitespace from headers
    df.columns = df.columns.str.strip()

    con = sqlite3.connect(f"../DockerCleaner/package-versions/{package_manager}.db")

    cur = con.cursor()
    cur.execute('DROP TABLE IF EXISTS Packages;')
    cur.execute('CREATE TABLE Packages (id INTEGER PRIMARY KEY, distro TEXT, package TEXT, version TEXT, modified_date TIMESTAMP);')
    cur.execute('CREATE INDEX file_hash_list_filesize_idx ON Packages (distro, package, version);')


    #cur.execute('DROP TABLE IF EXISTS Packages_fts;')
    #cur.execute('CREATE VIRTUAL TABLE Packages_fts USING fts5(distro, package, version, modified_date, tokenize="porter unicode61");')

    # drop data into database
    #cur.executemany('INSERT INTO Packages_fts (distro, package, version, modified_date) values (?,?,?,?);', df[['distro', 'package','version', 'modified_date']].to_records(index=False))
    #con.commit()

    df.to_sql("Packages", con, if_exists='append', index=False)


    #cur.execute("SELECT * FROM Packages WHERE distro ='bionic' and package IN ('runit','wget','chrpath','tzdata','man','lsof','lshw','sysstat','net-tools','numactl','python-httplib2') ORDER BY modified_date DESC")
    #cur.execute("SELECT * FROM Packages_fts WHERE distro MATCH ? and package LIKE ? ORDER BY modified_date DESC", ("bionic", "curl"))
    #rows = cur.fetchall()
    #for row in rows:
    #    print(row)
    con.close()

if __name__ == "__main__":
    convert_to_sql("apt-packages")
    convert_to_sql("apk-packages")