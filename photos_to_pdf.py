#!/usr/bin/env python3
"""Combine all images in a folder into a single PDF."""

from __future__ import annotations

import argparse
import re
from pathlib import Path

from PIL import Image

IMAGE_EXTENSIONS = {".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tif", ".tiff", ".webp"}


def natural_sort_key(path: Path) -> list[object]:
    parts = re.split(r"(\d+)", path.name.lower())
    return [int(part) if part.isdigit() else part for part in parts]


def load_image(path: Path) -> Image.Image:
    image = Image.open(path)
    if image.mode in ("RGBA", "LA", "P"):
        background = Image.new("RGB", image.size, (255, 255, 255))
        if image.mode == "P":
            image = image.convert("RGBA")
        background.paste(image, mask=image.split()[-1] if image.mode in ("RGBA", "LA") else None)
        return background
    if image.mode != "RGB":
        return image.convert("RGB")
    return image


def collect_images(input_dir: Path) -> list[Path]:
    images = [
        path
        for path in input_dir.iterdir()
        if path.is_file() and path.suffix.lower() in IMAGE_EXTENSIONS
    ]
    return sorted(images, key=natural_sort_key)


def photos_to_pdf(input_dir: Path, output_pdf: Path) -> int:
    image_paths = collect_images(input_dir)
    if not image_paths:
        raise FileNotFoundError(f"No images found in {input_dir}")

    pages = [load_image(path) for path in image_paths]
    first, rest = pages[0], pages[1:]
    output_pdf.parent.mkdir(parents=True, exist_ok=True)
    first.save(output_pdf, "PDF", resolution=100.0, save_all=True, append_images=rest)
    return len(image_paths)


def main() -> None:
    parser = argparse.ArgumentParser(description="Combine photos into a single PDF.")
    parser.add_argument(
        "input_dir",
        nargs="?",
        default="photos",
        help="Folder containing image files (default: photos)",
    )
    parser.add_argument(
        "-o",
        "--output",
        default="all_photos.pdf",
        help="Output PDF path (default: all_photos.pdf)",
    )
    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    output_pdf = Path(args.output)
    count = photos_to_pdf(input_dir, output_pdf)
    print(f"Created {output_pdf} with {count} page(s).")


if __name__ == "__main__":
    main()
