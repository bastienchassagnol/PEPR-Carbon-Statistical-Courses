"""Crop the twelve-panel reporting poster into one image per recommendation.

The source poster is a visual summary of Riley et al. (BMJ, 2022) and SAMPL.
Coordinates are in source pixels (1055 x 1491) and were read off the card
borders, not guessed as equal thirds.
"""

from pathlib import Path

from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "figures" / "sources" / "review-statistical-analysis.png"
OUT = ROOT / "figures" / "pitfalls"

# (y0, y1) for the four card rows, inclusive of the coloured header.
ROWS = [(140, 530), (539, 834), (841, 1129), (1137, 1441)]
# (x0, x1) for the three columns.
COLS = [(10, 338), (342, 670), (678, 1044)]
NAMES = [
    "01-question",
    "02-effects",
    "03-missing-data",
    "04-dichotomisation",
    "05-non-linearity",
    "06-subgroups",
    "07-heterogeneity",
    "08-prediction",
    "09-clustering",
    "10-variable-selection",
    "11-sensitivity",
    "12-reporting",
]


def main() -> None:
    image = Image.open(SOURCE).convert("RGB")
    OUT.mkdir(parents=True, exist_ok=True)
    index = 0
    for top, bottom in ROWS:
        for left, right in COLS:
            crop = image.crop((left, top, right, bottom))
            path = OUT / f"{NAMES[index]}.png"
            crop.save(path, optimize=True)
            print(f"{path.name} {crop.size}")
            index += 1


if __name__ == "__main__":
    main()
