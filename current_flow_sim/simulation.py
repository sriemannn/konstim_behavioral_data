import multiprocessing as mp
import os.path as op

import numpy as np
from alive_progress import alive_it
from simnibs import run_simnibs, sim_struct

cpus = mp.cpu_count()
cpus = 6 if (cpus > 6) else cpus


def get_list(possible_list):
    if "[" not in possible_list:
        no_list = possible_list
        return no_list
    possible_list = possible_list.replace("[", "").replace("]", "").split(",")
    try:
        possible_list = [float(i) for i in possible_list]
    except ValueError:
        print(f"Could not convert {possible_list} to float. Remains string.")
    return possible_list


def cm_to_mm(cm):
    return cm * 10


def return_current(current):
    return current * -1


if __name__ == "__main__":
    sim_parameter = np.loadtxt(
        "simulation_parameters.csv",
        delimiter="\t",
        dtype=str,
    )

    params = {}
    table_header = 0
    table_content_start = 1

    for index, col_name in enumerate(sim_parameter[table_header, :]):
        params[col_name] = sim_parameter[table_content_start:, index]

    for key in params.keys():
        try:
            params[key] = params[key].astype(float)
        except ValueError:
            pass

        if ("pos" in key) or (key == "dir"):
            params[key] = [get_list(i) for i in params[key]]

        if ("_x" in key) or ("_y" in key):
            params[key] = cm_to_mm(params[key])

    for idx, study in alive_it(enumerate(params["study"])):
        pathfem = op.join(
            ".",
            "simulation_results",
            f"{study}",
        )

        if op.exists(pathfem):
            print(f"Skipping {study} as it already exists.")
            continue

        s = sim_struct.SESSION()

        s.fnamehead = "MNI152.msh"  # from the simnibs example dataset
        s.pathfem = pathfem
        s.fields = "eEjJ"
        s.open_in_gmsh = False
        s.map_to_fsavg = True
        s.map_to_MNI = True

        tdcslist = s.add_tdcslist()

        current_strength = 1e-3  # 1 mA
        tdcslist.currents = [
            current_strength,
            return_current(current_strength),
        ]

        sponge = 3
        electrode = 1
        thicknesses_mm = [
            sponge,
            electrode,
            sponge,
        ]

        anode = tdcslist.add_electrode()
        anode.channelnr = 1
        anode.centre = params["anode_pos"][idx]
        anode.shape = "rect"
        anode.dimensions = [
            params["anode_x"][idx],
            params["anode_y"][idx],
        ]
        anode.thickness = thicknesses_mm

        cathode = tdcslist.add_electrode()
        cathode.channelnr = 2
        cathode.centre = params["cathode_pos"][idx]
        cathode.shape = "rect"
        cathode.dimensions = [
            params["cathode_x"][idx],
            params["cathode_y"][idx],
        ]
        cathode.thickness = thicknesses_mm
        cathode.pos_ydir = params["dir"][idx]

        run_simnibs(s, cpus=cpus)
