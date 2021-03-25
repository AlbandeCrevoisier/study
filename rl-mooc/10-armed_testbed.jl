### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ c6c4fdc2-82ad-11eb-044a-abace30e2eac
using Distributions, Plots, PlutoUI, Statistics

# ╔═╡ d0469884-7eba-11eb-1d19-eb3680e350a3
md"
# 10-armed Testbed

Non-associative k-armed bandit is the classical reinforcement learning toy problem.

Bandits are casino machines where one pulls a lever to obtain a reward, each iteration being independant from the previous ones. In the k-armed version, there are k-levers, each being associated with a reward function. The goal is to maximise the gains over a set period of time: this involves balancing exploration to estimate the expectation of each reward distribution as well as exploitation to maximise the gains — playing the lever associated with the highest estimated value.

A possible solution is found in ϵ-greedy methods: start with a set of prior value estimates, then at each step, pick a random action (lever) with probability ϵ, otherwise pick the greedy action (the highest estimated value).
* ϵ may be constant (useful to track changes in the reward function) or decreasing over time.
* Various strategies are possible to estimate values: often, one uses sample-average.

This testbed compares various values of ϵ on 2,000 10-armed bandits for 1,000 steps.

##### Notations

𝒜: set of actions\
q: state value\
r: reward\

##### 10-armed Bandit

∀ a ∈ 𝒜, q\*(a): true value, sampled from 𝒩(0, 1).\
Rewards: sampled from 𝒩(q\*(a), 1).


##### ϵ-greedy method

Initialisation: ∀ a ∈ 𝒜, q(a) = 0.\
p(ϵ): take a random action.\
p(1 - ϵ): take the greedy action.\
Update the action-value estimate with a sample-average.\
"

# ╔═╡ a317775a-8826-11eb-2c46-bfc7cdf28001
function makebandit(k=10)
	qₓ = rand(Normal(), k)
	a -> rand(Normal(qₓ[a]))
end

# ╔═╡ 55683178-82b0-11eb-21a0-118a2c5910ed
function makeϵgreedy(ϵ, k=10)
	# First row: sample-average.
	# Second row: count.
	sample_average = zeros(2, k)
	# Pick an action
	function f()
		# Explore
		if rand(Uniform()) < ϵ
			rand(1:k)
		else
			# Exploit
			argmax(sample_average[1, :])
		end
	end
	# Update sample-average value estimate.
	function f(a, r)
		prev, count = sample_average[:, a]
		sample_average[1, a] = (prev*count + r) / (count + 1)
		sample_average[2, a] = count + 1
	end
end

# ╔═╡ b9d1fd58-8830-11eb-2958-d93a88a14079
function playnsteps(bandit, ϵgreedy, n=1000)
	rewards = Array{Float64}(undef, n)
	for i in 1:n
		a = ϵgreedy()
		r = bandit(a)
		ϵgreedy(a, r)
		rewards[i] = r
	end
	rewards
end

# ╔═╡ 2af0f768-882f-11eb-11ca-39645ca19ff5
rewards = [mean([playnsteps(makebandit(), makeϵgreedy(ϵ)) for _ in 1:2000]) for ϵ in [0, 0.01, 0.1, 0.5]]

# ╔═╡ 1db37098-8a8b-11eb-0e02-993f6e332c7b
pgfplotsx()

# ╔═╡ 46ed3088-882f-11eb-1e36-b1766d7cf523
plot(1:1000, rewards, label = ["0" "0.01" "0.1" "0.5"])

# ╔═╡ adc5fd18-8d7e-11eb-2367-1906b97d68fc
md"### Nonstationary bandits"

# ╔═╡ 1849dc46-8d7e-11eb-2e96-ebbd410da0b9
function makenonstationarybandit(k=10)
	qₓ = zeros(k)
	a -> begin
		qₓ += rand(Normal(0, 0.01), k)
		rand(Normal(qₓ[a]))
	end
end

# ╔═╡ d066ae2e-8d7e-11eb-3c95-6b5ac74cd2af
nsrewards = [mean([playnsteps(makenonstationarybandit(), makeϵgreedy(ϵ), 10000) for _ in 1:2000]) for ϵ in [0, 0.01, 0.1, 0.5]]

# ╔═╡ f91cb75c-8d7e-11eb-23d4-83799d5f050f
plot(1:10000, nsrewards, label = ["0" "0.01" "0.1" "0.5"])

# ╔═╡ Cell order:
# ╟─d0469884-7eba-11eb-1d19-eb3680e350a3
# ╟─c6c4fdc2-82ad-11eb-044a-abace30e2eac
# ╟─a317775a-8826-11eb-2c46-bfc7cdf28001
# ╟─55683178-82b0-11eb-21a0-118a2c5910ed
# ╟─b9d1fd58-8830-11eb-2958-d93a88a14079
# ╟─2af0f768-882f-11eb-11ca-39645ca19ff5
# ╟─1db37098-8a8b-11eb-0e02-993f6e332c7b
# ╠═46ed3088-882f-11eb-1e36-b1766d7cf523
# ╟─adc5fd18-8d7e-11eb-2367-1906b97d68fc
# ╟─1849dc46-8d7e-11eb-2e96-ebbd410da0b9
# ╟─d066ae2e-8d7e-11eb-3c95-6b5ac74cd2af
# ╟─f91cb75c-8d7e-11eb-23d4-83799d5f050f
