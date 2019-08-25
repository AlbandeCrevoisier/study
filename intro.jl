ENV["MPLBACKEND"]="TkAgg"

using Gen, PyPlot

@gen function line_model(xs)
	slope = @trace(normal(0, 1), :slope)
	intercept = @trace(normal(0, 2), :intercept)
	return slope * xs .+ intercept
end;

@gen function sine_model(xs)
	phase = @trace(uniform(0, 2 * pi), :phase)
	period = @trace(gamma(5, 1), :period)
	amplitude = @trace(gamma(1, 1), :amplitude)
	return amplitude * sin.(2 * pi * xs / period .+ phase)
end;

@gen function combined_model(xs)
	if @trace(bernoulli(0.5), :is_line)
		ys = @trace(line_model(xs))
	else
		ys = @trace(sine_model(xs))
	end
	noise = @trace(gamma(1, 1), :noise)
	for (i, y) in enumerate(ys)
		@trace(normal(y, noise), (:y, i))
	end
end;

function render_combined_trace!(trace, ax)
	xs = get_args(trace)[1]
	xmin = minimum(xs)
	xmax = maximum(xs)
	ys = [trace[(:y, i)] for i=1:length(xs)]
	ymin = minimum(ys)
	ymax = maximum(ys)

	ax.scatter(xs, ys)
	if trace[:is_line]
		ax.plot([xmin, xmax],
			trace[:slope] * [xmin, xmax] .+ trace[:intercept])
	else
		pts = collect(range(xmin, stop=xmax, length=100))
		ax.plot(pts, trace[:amplitude] *
			sin.(2 * pi * pts / trace[:period] .+ trace[:phase]))
	end
	ax.set_xlim(xmin, xmax)
	ax.set_ylim(ymin, ymax)
end;

function infer(model, xs, ys, amount_of_computation)
	observations = choicemap()
	for (i, y) in enumerate(ys)
		observations[(:y, i)] = y
	end
	
	(trace, _) = importance_resampling(model, (xs,), observations,
		amount_of_computation);
	return trace
end;

function predict(model, trace, xs, param_addrs)
	constraints = choicemap()
	for addr in param_addrs
		constraints[addr] = trace[addr]
	end
	(new_trace, _) = generate(model, (xs,), constraints)
	return new_trace
end;

fig, axs = plt.subplots(3, 3, figsize=(9, 9))
xs = collect(range(-10, stop=10))
sine_ys = 0.5 * sin.(2 * pi * xs .+ pi / 2)
pxs = collect(range(-20, stop=20))
for ax in axs
	it = infer(combined_model, xs, sine_ys, 100000)
	if it[:is_line]
		params_addr = [:slope, :intercept, :noise]
	else
		params_addr = [:amplitude, :period, :phase, :noise]
	end
	nt = predict(combined_model, it, pxs, params_addr)
	render_combined_trace!(nt, ax)
end;
plt.show()
