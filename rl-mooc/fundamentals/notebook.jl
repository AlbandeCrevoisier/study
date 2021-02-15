### A Pluto.jl notebook ###
# v0.12.10

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ c40545ca-2500-11eb-0ffb-d19c7d8bd6c2
using Plots

# ╔═╡ 1921d542-2807-11eb-0503-a925e05a89eb
using PlutoUI

# ╔═╡ e5c19e5e-22bf-11eb-0ec2-051a6eb67737
md"
# Reinforcement Learning

### An Introduction
#### second edition

Richard S. Sutton and Andrew G. Barto
"

# ╔═╡ a1b09e3c-22ca-11eb-2ed4-49df077c554b
md"### Chapter 3: Finite Markov Decision Processes"

# ╔═╡ cb05c33e-22ca-11eb-3bae-c52efe45155a
md"
##### Example 3.5: Gridworld

State: current cell. \
Four actions: North, South, East, West.

Reward:
- default: 0, goes to indicated cell.
- going offbounds: -1, stays in place.
- A (resp. B): any action goes to A' (resp. B')

 . |  A  | . |  B  | .
---|:---:|---|:---:|---
 . | \|  | . |  ↓  | +5
 . | \|  |+10|  B' | .
 . |  ↓  | . |  .  | .
 . |  A' | . |  .  | .
"

# ╔═╡ 8940833e-22ce-11eb-3696-f18916c362b7
A = [1, 2]

# ╔═╡ e7a41abc-22ce-11eb-20df-a7bf807effe7
APrime = [5, 2]

# ╔═╡ e7a44e7e-22ce-11eb-37ff-c1f6cf5885a5
B = [1, 4]

# ╔═╡ e7a4e14a-22ce-11eb-2767-ef965e49db48
BPrime = [3, 4]

# ╔═╡ 6c3114de-22ce-11eb-2469-b91374913278
function mdp(s, a)
	# mdp: s, a -> s', r
	if s == A
		return APrime, 10
	elseif s == B
		return BPrime, 5
	elseif ((s[1] == 1 && a == :North) ||
		    (s[1] == 5 && a == :South) ||
			(s[2] == 1 && a == :West) ||
			(s[2] == 5 && a == :East))
		return s, -1
	elseif a == :North
		return s - [1, 0], 0
	elseif a == :South
		return s + [1, 0], 0
	elseif a == :East
		return s + [0, 1], 0
	elseif a == :West
		return s - [0, 1], 0
	else
		error("Impossible!")
	end
end

# ╔═╡ f9fb530a-22cf-11eb-1407-63170243edad
begin
	@assert mdp(A, :North) == (APrime, 10)
	@assert mdp(B, :North) == (BPrime, 5)
	@assert mdp([1, 1], :North) == ([1, 1], -1)
	@assert mdp([2, 2], :North) == ([1, 2], 0)
	@assert mdp([2, 2], :South) == ([3, 2], 0)
	@assert mdp([2, 2], :East) == ([2, 3], 0)
	@assert mdp([2, 2], :West) == ([2, 1], 0)
end

# ╔═╡ a411fd46-24f9-11eb-32b3-759c71e56181
@bind γ Slider(0.7:0.05:1; default=0.9, show_value=true)

# ╔═╡ acdc29fe-2807-11eb-0a9c-1f33224253d1
@bind nIter Slider(100000:100000:10000000, default=100000, show_value=true)

# ╔═╡ bc20cbc8-24f5-11eb-0de8-21e8b2050306
# Stochastic iterative policy evaluation
let
	value = fill(1., (5, 5))
	for i in 1:nIter
		s = rand(1:5, 2)
		values = Dict()
		for a in [:North, :South, :East, :West]
			sPrime, r = mdp(s, a)
			values[a] = r + γ * value[sPrime[1], sPrime[2]]
		end
		value[s[1], s[2]] = values[argmax(values)]
	end
	heatmap(value, yflip=true)
end

# ╔═╡ Cell order:
# ╟─e5c19e5e-22bf-11eb-0ec2-051a6eb67737
# ╟─a1b09e3c-22ca-11eb-2ed4-49df077c554b
# ╟─cb05c33e-22ca-11eb-3bae-c52efe45155a
# ╟─8940833e-22ce-11eb-3696-f18916c362b7
# ╟─e7a41abc-22ce-11eb-20df-a7bf807effe7
# ╟─e7a44e7e-22ce-11eb-37ff-c1f6cf5885a5
# ╟─e7a4e14a-22ce-11eb-2767-ef965e49db48
# ╟─6c3114de-22ce-11eb-2469-b91374913278
# ╟─f9fb530a-22cf-11eb-1407-63170243edad
# ╟─a411fd46-24f9-11eb-32b3-759c71e56181
# ╟─acdc29fe-2807-11eb-0a9c-1f33224253d1
# ╠═bc20cbc8-24f5-11eb-0de8-21e8b2050306
# ╠═c40545ca-2500-11eb-0ffb-d19c7d8bd6c2
# ╠═1921d542-2807-11eb-0503-a925e05a89eb
