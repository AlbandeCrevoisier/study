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

@gen function agent_model(scene::Scene, dt::Float64, num_ticks::Int, planner_params::PlannerParams)

    # sample the start point of the agent from the prior
    start_x = @trace(uniform(0, 1), :start_x)
    start_y = @trace(uniform(0, 1), :start_y)
    start = Point(start_x, start_y)

    # sample the destination point of the agent from the prior
    dest_x = @trace(uniform(0, 1), :dest_x)
    dest_y = @trace(uniform(0, 1), :dest_y)
    dest = Point(dest_x, dest_y)

    # plan a path that avoids obstacles in the scene
    maybe_path = plan_path(start, dest, scene, planner_params)
    planning_failed = maybe_path == nothing

    # sample the speed from the prior
    speed = @trace(uniform(0, 1), :speed)

    if planning_failed
        # path planning failed, assume the agent stays as the start location indefinitely
        locations = fill(start, num_ticks)
    else
        # path planning succeeded, move along the path at constant speed
        locations = walk_path(maybe_path, speed, dt, num_ticks)
    end

    # generate noisy measurements of the agent's location at each time point
    noise = 0.01
    for (i, point) in enumerate(locations)
        x = @trace(normal(point.x, noise), :meas => (i, :x))
        y = @trace(normal(point.y, noise), :meas => (i, :y))
    end

    return (planning_failed, maybe_path)
end

function trace_to_dict(trace)
    args = Gen.get_args(trace)
    (scene, dt, num_ticks, planner_params) = args
    choices = Gen.get_choices(trace)
    (planning_failed, maybe_path) = Gen.get_retval(trace)

    d = Dict()

    # scene (the obstacles)
    d["scene"] = scene

    # the points along the planned path
    if planning_failed
        d["path"] = []
    else
        d["path"] = maybe_path.points
    end

    # start and destination location
    d["start"] = Point(choices[:start_x], choices[:start_y])
    d["dest"] = Point(choices[:dest_x], choices[:dest_y])

    # the observed location of the agent over time
    measurements = Vector{Point}(undef, num_ticks)
    for i=1:num_ticks
        measurements[i] = Point(choices[:meas => (i, :x)], choices[:meas => (i, :y)])
    end
    d["measurements"] = measurements

    return d
end


speed = 1.
dt = 0.1
num_ticks = 10;
planner_params = PlannerParams(300, 3.0, 2000, 1.)

constraints = choicemap()
constraints[:start_x] = 0.1
constraints[:start_y] = 0.1

viz = Viz(viz_server, joinpath(@__DIR__, "qs/inverse-planning/grid-viz/dist"), [])
for i=1:12
    (trace, _) = generate(agent_model, (scene, dt, num_ticks, planner_params), constraints)
    putTrace!(viz, i, trace_to_dict(trace))
end
openInBrowser(viz)

sleep(999999)
