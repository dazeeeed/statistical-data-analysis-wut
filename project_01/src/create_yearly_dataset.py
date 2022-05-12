import requests
import os
from tqdm import tqdm
import shutil

YEAR = "2021"

def download_zip_data(url: str, path: str):
    for i in tqdm(range(1, 13)):
        file_name = YEAR + "_" + "{:02d}".format(i) + "_k.zip"
        file = url + file_name
        if not os.path.exists(os.path.join(path, file_name)):
            response = requests.get(file)
            open(os.path.join(path, file_name), "wb").write(response.content)


def unzip_data(path: str):
    for i in tqdm(range(1, 13)):
        path_zip_file = os.path.join(path, YEAR + "_" + "{:02d}".format(i) + "_k.zip")
        extract_folder = os.path.join(path, YEAR + "_" + "{:02d}".format(i))
        shutil.unpack_archive(path_zip_file, extract_folder)


def collect_data_to_csv(path: str):
    with open(os.path.join(path, "k_d_" + YEAR + ".csv"), 'w') as result_file:
        for i in tqdm(range(1, 13)):
            extract_folder = os.path.join(path, YEAR + "_" + "{:02d}".format(i))
            file_name = "k_d_" + "{:02d}".format(i) + "_" + YEAR + ".csv"
            with open(os.path.join(extract_folder, file_name), 'r') as read_file:
                result_file.writelines(read_file.readlines())


def main():

    URL_BASE = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/" + \
               YEAR + "/"
    PATH_CURRENT = os.path.dirname(os.path.realpath(__file__))
    PATH_DATA = os.path.join(PATH_CURRENT, "..", "data")
    PATH_DATA_YEAR = os.path.join(PATH_DATA, YEAR)

    if not os.path.exists(PATH_DATA_YEAR):
        os.mkdir(PATH_DATA_YEAR)

    download_zip_data(URL_BASE, PATH_DATA_YEAR)
    unzip_data(PATH_DATA_YEAR)
    collect_data_to_csv(PATH_DATA_YEAR)


if __name__=="__main__":
    main()
