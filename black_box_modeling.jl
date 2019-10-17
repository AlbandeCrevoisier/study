using Gen, GenViz

viz_server = VizServer(8090)
sleep(1)

include("qs/inverse-planning/geometric_primitives.jl")
include("qs/inverse-planning/scene.jl")
include("qs/inverse-planning/planning.jl")

scene = Scene(0, 1, 0, 1)
add_obstacle!(scene, make_square(Point(0.30, 0.20), 0.1))
add_obstacle!(scene, make_square(Point(0.83, 0.80), 0.1))
add_obstacle!(scene, make_square(Point(0.80, 0.40), 0.1))
horizontal = false
vertical = true
wall_thickness = 0.02
add_obstacle!(scene, make_line(horizontal, Point(0.20, 0.40), 0.40, wall_thickness))
add_obstacle!(scene, make_line(vertical, Point(0.60, 0.40), 0.40, wall_thickness))
add_obstacle!(scene, make_line(horizontal, Point(0.60 - 0.15, 0.80), 0.15 + wall_thickness, wall_thickness))
add_obstacle!(scene, make_line(horizontal, Point(0.20, 0.80), 0.15, wall_thickness))
add_obstacle!(scene, make_line(vertical, Point(0.20, 0.40), 0.40, wall_thickness))

start = Point(0.1, 0.1)
dest = Point(0.5, 0.5)
planner_params = PlannerParams(300, 3.0, 2000, 1.)
path = plan_path(start, dest, scene, planner_params)

info = Dict("start"=> start, "dest" => dest, "scene" => scene, "path_edges" => get_edges(path))
viz_pa = Viz(viz_server, joinpath(@__DIR__, "qs/inverse-planning/overlay-viz/dist"), info)
openInBrowser(viz_pa)
sleep(999999)
