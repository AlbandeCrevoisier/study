using Gen
using PyPlot

@gen function doubleaddr()
	@trace(normal(0, 1), :x)
	@trace(normal(0, 1), :x)
end;

@gen function sine(xs::Vector{float})
	phase = @trace(uniform(-pi, pi), :phase)
	period = @trace(gamma(5, 1), :period)
	amplitude = @trace(gamma(1, 1), :amplitude)
	for (i, x) in enumerate(xs)
		@trace(normal(amplitude * sin(x * period / (2 * pi) + phase), 0.1), (:y, i))
	end
end;

@gen function render_sine(trace)
	xs = get_args(trace)[1]
	ys = [trace[(:y, i)] for i=1:length(xs)]
	xmin = minimum(xs)
	xmax = maximum(xs)
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
	plt.show()
end;

figure(figsize=(3, 3))
t = Gen.simulate(sine, (range(0, stop=20),))
render_sine(t);
