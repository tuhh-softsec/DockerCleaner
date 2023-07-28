from bs4 import BeautifulSoup
import requests


def crawl(dfile, url):
    print(url)
    page = requests.get(url + "?C=M;O=A").text
    soup = BeautifulSoup(page, 'html.parser')
    directories = [node.get('href') for node in soup.find_all('a') if not node.get('href').startswith("/") and node.get('href').endswith("/")]
    deb_files = [(node.get('href'), node.parent.nextSibling.text.strip()) for node in soup.find_all('a') if node.get('href').endswith("all.deb") or node.get('href').endswith("amd64.deb")]

    for deb in deb_files:
        dfile.write(f"{deb[0]},{deb[1]}\n")
    
    dfile.flush()

    for dir in directories:
        crawl(dfile, url + dir)

if __name__ == "__main__":
    #f = open("ubuntu_package_dates.csv", "a")
    #crawl(f, "http://de.archive.ubuntu.com/ubuntu/pool/main/")
    #crawl(f, "http://de.archive.ubuntu.com/ubuntu/pool/universe/")
    #crawl(f, "http://de.archive.ubuntu.com/ubuntu/pool/multiverse/")

    #f = open("debian_package_dates.csv", "a")
    #crawl(f, "http://ftp.de.debian.org/debian/pool/main/")
    #crawl(f, "http://ftp.de.debian.org/debian/pool/contrib/")
    #crawl(f, "http://ftp.de.debian.org/debian/pool/non-free/")

    f = open("debian_security_package_dates.csv", "a")
    crawl(f, "https://security.debian.org/debian-security/pool/main/")
    crawl(f, "https://security.debian.org/debian-security/pool/contrib/")
    crawl(f, "https://security.debian.org/debian-security/pool/non-free/")
    f.close()