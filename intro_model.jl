ENV["MPLBACKEND"]="TkAgg"

using Gen, PyPlot

# 2 - Exercise
@gen function doubleaddr()
	@trace(normal(0, 1), :x)
	@trace(normal(0, 1), :x)
end;

# 2 - Exercise (2)
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

# 3 - Exercise
xs = collect(range(-5., stop=5.))
ys_sine = [2.89, 2.22, -0.612, -0.522, -2.65, -0.133, 2.70, 2.77, 0.425, -2.11,
	-2.76];

function infer_sine(xs, ys, amount_of_computation)
	observations = Gen.choicemap()
	for (i, y) in enumerate(ys)
		observations[(:y, i)] = y
	end
	(trace, _) = Gen.importance_resampling(sine, (xs, ), observations,
		amount_of_computation);
	return trace;
end;

t = infer_sine(xs, ys_sine, 100)
render_sine(t)
show()
