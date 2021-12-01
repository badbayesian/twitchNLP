"""
"""

from bs4 import BeautifulSoup
import html2text
import os
from pathlib import Path
from tqdm import tqdm
from multiprocessing import Pool
import argh


def html_to_txt(html_name: str, file_loc: str) -> None:
    """Translates html from vod.online to a .txt file

    Args:
        html_name (str): html_name
        file_loc (str): file_loc

    Returns:
        None:
    """


    name, _ = html_name.split(".")

    address = f"{file_loc}/{html_name}"


    html_page = open(f"{address}", 'r')
    soup = BeautifulSoup(html_page, "html.parser")

    h = html2text.HTML2Text()
    h.ignore_links = True

    with open(f"{file_loc}/translated_{name}.txt", "w") as f:
        for line in h.handle(str(soup)):
            f.write(line)

def main(file_loc, cores=16) -> None:
    """main.

    Args:
        file_loc:
        cores:

    Returns:
        None:
    """
    files = os.listdir(file_loc)

    if cores > 1:
        with Pool(processes=cores) as pool:
            jobs = [pool.apply_async(func=html_to_txt, args=(f, file_loc)) for f in files if Path(f).suffix == ".html"]
            results = [job.get() for job in tqdm(jobs, desc="Translating .html to .txt")]
    else:
        results = [html_to_txt(f, file_loc) for f in tqdm(jobs, desc="Translating .html to .txt") if Path(f).suffix == ".html"]


if __name__ == "__main__":
    argh.dispatch_command(main)
