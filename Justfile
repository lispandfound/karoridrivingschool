# Define directories
dist := "dist"
src  := "website"

reset:
    rm -rf {{dist}}
    cp -r {{src}} {{dist}}

# The logic encapsulated in Python
build: reset
  #!/usr/bin/env python3
  import os
  import hashlib
  import glob
  from pathlib import Path

  dist_dir = "{{dist}}"
  manifest = {}
  static_dir = Path(dist_dir) / 'static'
  for filepath in static_dir.rglob("*"):
      content = filepath.read_bytes()
      file_hash = hashlib.md5(content).hexdigest()[:8]

      new_name = f"{filepath.stem}.{file_hash}{filepath.suffix}"
      new_path = filepath.with_name(new_name)

      filepath.rename(new_path)
      manifest[filepath.name] = new_name
      print(f"Renamed: {filepath.name} -> {new_name}")

  for html_file in Path(dist_dir).rglob("*.html"):
      content = html_file.read_text()
      for original, hashed in manifest.items():
          content = content.replace(original, hashed)
      html_file.write_text(content)
      print(f"Updated references in: {html_file.name}")

  for css_file in Path(dist_dir).rglob("*.css"):
      content = css_file.read_text()
      for original, hashed in manifest.items():
          content = content.replace(original, hashed)
      css_file.write_text(content)
      print(f"Updated references in: {css_file.name}")


deploy: build
    cd dist && rsync -r --delete . /var/www/karoridrivingschool.co.nz/
