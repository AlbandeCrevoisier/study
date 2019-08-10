using Gen

@gen function doubleaddr()
	@trace(normal(0, 1), :x)
	@trace(normal(0, 1), :x)
end;

@gen function sine(xs::Vector{float})
	n = length(xs)
	phase = @trace(uniform(-pi, pi), :phase)
	period = @trace(gamma(5, 1), :period)
	amplitude = @trace(gamma(1, 1), :amplitude)
	for (i, x) in enumerate(xs)
		@trace(normal(amplitude * sin(x * period / (2 * pi) + phase), 0.1), (:y, i))
	end
end;
