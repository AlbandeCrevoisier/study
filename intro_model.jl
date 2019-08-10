using Gen
using PyPlot

# Exercise 1
@gen function doubleaddr()
	@trace(normal(0, 1), :x)
	@trace(normal(0, 1), :x)
end;

# Exercise 2
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
	ax.set_ylim((- amplitude, amplitude))
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

xs = range(1, stop=10)
traces = [Gen.simulate(sine, (xs,)) for _=1:12]
sine_grid(render_sine, traces, ncols=4, nrows=3)
