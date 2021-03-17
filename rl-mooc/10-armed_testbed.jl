### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ c6c4fdc2-82ad-11eb-044a-abace30e2eac
using Distributions, Plots

# ╔═╡ d0469884-7eba-11eb-1d19-eb3680e350a3
md"
# 10-armed Testbed

Non-associative k-armed bandit is the classical reinforcement learning toy problem.

Bandits are casino machines where one pulls a lever to obtain a reward, each iteration being independant from the previous ones. In the k-armed version, there are k-levers, each being associated with a reward function. The goal is to maximise the gains over a set period of time: this involves balancing exploration to estimate the expectation of each reward distribution as well as exploitation to maximise the gains — playing the lever associated with the highest estimated value.

A possible solution is found in ϵ-greedy methods: start with a set of prior value estimates, then at each step, pick a random action (lever) with probability ϵ, otherwise pick the greedy action (the highest estimated value).
* ϵ may be constant (useful to track changes in the reward function) or decreasing over time.
* Various strategies are possible to estimate values: often, one uses sample-average.

This testbed compares various values of ϵ on 2,000 10-armed bandits.

##### 10-armed Bandit

q\*(a): true value, sampled from 𝒩(0, 1).\
Rewards: sampled from 𝒩(q\*(a), 1).


##### ϵ-greedy method

Initialisation: ∀ a ∈ 𝒜, q(a) = 0.\
p(ϵ): take a random action.\
p(1 - ϵ): take the greedy action.\
Update the action-value estimate with a sample-average.\
"

# ╔═╡ c94c7f8e-82ad-11eb-0aa8-7ff748d044f1
qₓ = rand(Normal(), 10)

# ╔═╡ 55683178-82b0-11eb-21a0-118a2c5910ed
function make_ϵ_greedy(ϵ)
	function f()
		if rand(Uniform()) < ϵ
			rand(1:10, 1)
		else
			# Take the greedy action
			1  # temporary
		end
	end
end

# ╔═╡ 98f15cf6-82b4-11eb-3618-215e3f8fa2a4
f = make_ϵ_greedy(0.1)

# ╔═╡ Cell order:
# ╟─d0469884-7eba-11eb-1d19-eb3680e350a3
# ╟─c6c4fdc2-82ad-11eb-044a-abace30e2eac
# ╟─c94c7f8e-82ad-11eb-0aa8-7ff748d044f1
# ╠═55683178-82b0-11eb-21a0-118a2c5910ed
# ╠═98f15cf6-82b4-11eb-3618-215e3f8fa2a4
