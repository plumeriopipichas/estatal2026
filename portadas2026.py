#!/usr/bin/python
# coding: utf-8

import pandas as pd
import os
import subprocess

# Lista de sedes sin acentos ni espacios (para nombres de archivo .tex)
sedes = ["Actopan", "Sahagun", "Huejutla", "Huichapan", "Ixmiquilpan",
         "Ixtlahuaco", "Metztitlan", "Pachuca CECYTEH", "Pachuca ITESM",
         "Pachuca UAEH", "Pisaflores", "Tizayuca",
         "Tlanchinol", "Tula", "Tulancingo", "Zimapan"]

# Nombres bonitos para portada
nombres = ["Actopan", "Ciudad Sahagún", "Huejutla", "Huichapan", "Ixmiquilpan",
           "Ixtlahuaco", "Metztitlán", "Pachuca CECYTEH", "Pachuca ITESM",
           "Pachuca UAEH", "Pisaflores", "Tizayuca",
           "Tlanchinol", "Tula", "Tulancingo", "Zimapán"]

sedesdict = dict(zip(sedes, nombres))

# Directorio de los .csv
directorio = '../../listas_generadas/listas_de_asistencia/'

# Preambulo con fontspec y Droid Sans
preamble = """\\documentclass{article}
\\usepackage{portadas2025}
\\usepackage{fontspec}
\\usepackage{geometry}
\\setmainfont{Droid Sans}
\\geometry{margin=2.6cm}
\\begin{document}
\\pagestyle{empty}
"""

# Plantilla para cada alumno
template = """\\Portada[alumno={{{alumno}}}, equipo={{{equipo}}},"""
template += " sede={sede}, clave={clave}]\n"

# Generar .tex para cada sede
for lugar in sedes:
    texfile = f"{lugar}.tex"
    with open(texfile, 'w') as t:
        t.write(preamble)
        df = pd.read_csv(directorio + f'{lugar}_asistencia.csv')
        df = df.fillna('')  # <- Esto soluciona lo del "nan"
        for _, row in df.iterrows():
            temp_alumno = f"{row['Nombre']} {row['Primer_apellido']} {row['Segundo_apellido']}"
            renglon = template.format(
                alumno=temp_alumno,
                equipo=row['Equipo'],
                sede=sedesdict[row['Sede']],
                clave=row['clave']
            )
            t.write(renglon)
        t.write("\\end{document}")

    # Compilar .tex → .pdf con XeLaTeX (silencioso)
    subprocess.run(
        ["xelatex", "-interaction=nonstopmode", texfile],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )

    # Limpiar archivos intermedios
    base = texfile.replace(".tex", "")
    for ext in [".aux", ".log"]:
        try:
            os.remove(base + ext)
        except FileNotFoundError:
            pass
