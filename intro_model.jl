ENV["MPLBACKEND"]="TkAgg"

using Gen, PyPlot

# Part 2 - Exercise
@gen function doubleaddr()
	@trace(normal(0, 1), :x)
	@trace(normal(0, 1), :x)
end;

# Part 2 - Exercise (2)
@gen function sine(xs::Vector{float})
	phase = @trace(uniform(-pi, pi), :phase)
	period = @trace(gamma(5, 1), :period)
	amplitude = @trace(gamma(1, 1), :amplitude)
	for (i, x) in enumerate(xs)
		mu = amplitude * sin(2 * pi * x / period + phase)
		@trace(normal(mu, 0.1), (:y, i))
	end
end;

function render_sine(trace)
	xs = get_args(trace)[1]
	xmin = minimum(xs)
	xmax = maximum(xs)
	ys = [trace[(:y, i)] for i=1:length(xs)]
	phase = trace[:phase]
	period = trace[:period]
	amplitude = trace[:amplitude]
	x = range(xmin, xmax, step=0.1)
	y = amplitude * sin.(x * period / (2 * pi) .+ phase)

	scatter(xs, ys)
	ax = gca()
	ax.set_xlim((xmin, xmax))
	ax.set_ylim((
		minimum([-amplitude, minimum(ys)]),
		maximum([amplitude, maximum(ys)])))
	plot(x, y, alpha=0.5)
end;

function sine_grid(renderer::Function, traces; ncols=6, nrows=3)
	figure(figsize=(3 * ncols, 3 * (nrows + 1)))
	for (i, trace) in enumerate(traces)
		subplot(nrows, ncols, i)
		renderer(trace)
	end
	show()
end;

# Part 3 - Exercise
xs = collect(range(-5., stop=5.))
ys_sine = [2.89, 2.22, -0.612, -0.522, -2.65, -0.133, 2.70, 2.77, 0.425, -2.11,
	-2.76];

function infer_sine(xs, ys, amount_of_computation)
	observations = choicemap()
	for (i, y) in enumerate(ys)
		observations[(:y, i)] = y
	end
	(trace, _) = importance_resampling(sine, (xs, ), observations,
		amount_of_computation);
	return trace;
end;

# Part 5 - Exercise (1)
# First, implement some functions from the notebook.
@gen function refactored_line(xs::Vector{float})
	ys = []
	slope = @trace(normal(0, 1), :slope)
	intercept = @trace(normal(0, 2), :intercept)
	for (i, x) in enumerate(xs)
		push!(ys, slope * x + intercept)
	end
	return ys
end;

@gen function refactored_sine(xs::Vector{float})
	ys = []
	phase = @trace(uniform(0, 2 * pi), :phase)
	period = @trace(gamma(5, 1), :period)
	amplitude = @trace(gamma(1, 1), :amplitude)
	for (i, x) in enumerate(xs)
		push!(ys, amplitude * sin(2 * pi * x / period + phase))
	end
	return ys
end;

@gen function refactored_combined_model(xs::Vector{float})
	if @trace(bernoulli(0.5), :is_line)
		ys = refactored_line(xs)
	else
		ys = refactored_sine(xs)
	end
	noise = @trace(gamma(1, 1), :noise)
	for (i, y) in enumerate(ys)
		@trace(normal(y, noise), (:y, i))
	end
end;

function do_inference(model, xs, ys, amount_of_computation)
	observations = choicemap()
	for (i, y) in enumerate(ys)
		observations[(:y, i)] = y
	end
	
	(trace, _) = importance_resampling(model, (xs,), observations,
		amount_of_computation);
	return trace
end;

ambiguous_xs = [0., pi, 2 * pi, 3 * pi, 4 * pi, 5 * pi]
ambiguous_ys = [0., 0., 0., 0., 0., 0.]

function post_proba_is_sine(xs, ys)
	post_proba = 0.
	for _=1:100
		trace = do_inference(refactored_combined_model,
			xs, ys, 10000)
		if trace[:is_line]
			post_proba += 1.
		end
	end
	return post_proba / 100.
end;

pp = post_proba_is_sine(ambiguous_xs, ambiguous_ys)
println(pp)
