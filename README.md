# Selbsteinschätzung der Qualität in Zürcher Kitas

A Shiny dashboard for Zurich daycare centres (Kitas) to self-assess their quality, developed by the [ZHAW Qualitätsfachstelle für Zürcher Kitas](https://www.zhaw.ch/de/sozialearbeit/qualitaet-in-zuercher-kitas).

Data is fetched live from REDCap. Credentials are passed via environment variables `REDCAP_TOKEN` and `REDCAP_URL` (or a local `config/secrets.yml` for development).