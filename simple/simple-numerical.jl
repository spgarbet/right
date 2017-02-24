################################################
# Packages Required
# Pkg.add("DifferentialEquations")
# Pkg.add("DataFrames")
# Pkg.add("Dierckx")
# Pkg.add("Plots")

# From: Christopher Rackauckas
# If the problem was stiff, I think the best option right now is the algorithm
# alg = MethodOfSteps(Rosenbrock23()). Rosenbrock23() on ODEs isn't as fast as
# using LSODA, but you can't use LSODA in MethodOfSteps. 

using DataFrames
using DifferentialEquations
using Dierckx
using Plots

# Read 2011 SS death tables
ss_death = readtable("ss-death-2011.csv")

# Instantaneous exponential rate from percent over some time frame
instantaneous_rate(percent, timeframe) = - log(1-percent) / timeframe

################################################
# Parameters



#################################################
# Determine death rate function via spline
f_40yr_percent_d    = ss_death[:_f_death_prob][41:120]

sim_adj_age         = [0:79...] + 0.5 # 0.5 offset since percentage is for whole year
f_40yr_death_spline = Spline1D(sim_adj_age, f_40yr_percent_d)

# Test plot
# plot(sim_adj_age, f_40yr_death_spline(sim_adj_age))

f_40yr_death_rate(t) = instantaneous_rate(f_40yr_death_spline(t), 1)


####################################
# Integrations of death rates for exposure calculations in delay
# Now, a special function used in delay equation (Had to put upper bound at 81)
f_40yr_death_rate_5yr(t, r_b) = exp(-5.0*r_b-quadgk(f_40yr_death_rate, maximum([t-5, 0]), minimum([t, 81]))[1])
f_40yr_death_rate_1yr(t)      = exp(-quadgk(f_40yr_death_rate, maximum([t-1, 0]), minimum([t, 81]))[1])

####################################
# Define Model

function simple(t, y, h, p, dy)
  r_a  = p[1]
  r_b  = p[2]
  r_ad = p[3]
  r_d = f_40yr_death_rate(t)

  dd_b =  if (t >= 5.0 && t <= 10.0)
            (1-r_ad)*r_a*f_40yr_death_rate_5yr(t,r_b)*(h(t-5)[1])
          else
            0.0
          end

  # Local rate of a is function of time (it goes off at 5)
  lr_a = if(t > 5.0) 0.0 else r_a end

  dy[1] = -(lr_a+r_d)*y[1]                                # H bucket
  dy[2] = lr_a*y[1]                                       # A bucket
  dy[3] = (1-r_ad)*lr_a*y[1] - (r_b+r_d)*y[3] - dd_b      # E10 Bucket
  dy[4] = dd_b - r_d*y[4]                                 # E15 Bucket
  dy[5] = r_b*y[3] - r_d*y[5]                             # E20 Bucket
  dy[6] = r_b*y[3]                                        # B bucket
  dy[7] = r_b*y[3] - r_d*y[7]                             # Disutility of B
  
  if(y[3] < 0) 
    dy[3] = -y[3]
  end
  
  if(t>=1 && t <= 11) 
    dy[7] = dy[7] - (r_b*h(t-1)[3])*f_40yr_death_rate_1yr(t)
  end
end
            
pf_simple = DDEParameterizedFunction(
  simple,
  [instantaneous_rate(0.1, 5), instantaneous_rate(0.5, 5),  0.05])

hh(t) = zeros(7)

prob = ConstantLagDDEProblem(
  pf_simple,
  hh,
  vcat([1.0], zeros(6)),
  [1,5],
  (0.0, 80.0))
 
########## How to run 
sol=solve(prob, MethodOfSteps(Tsit5()))
plot(sol)

function discounted_a(sol, discount, a_rate)
  quadgk(function(t) a_rate*(sol(t)[1])*exp(-discount * t) end, 0, 5)[1]
end

function discounted_b(sol, discount, b_rate)
  quadgk(function(t) b_rate*(sol(t)[3])*exp(-discount * t) end, 0, 10)[1]
end

function disutility_a_time(sol, discount)
  quadgk(function(t) sol(t)[3]*exp(-discount*t) end, 0, 80)[1] +
  quadgk(function(t) sol(t)[4]*exp(-discount*t) end, 0, 80)[1] +
  quadgk(function(t) sol(t)[5]*exp(-discount*t) end, 0, 80)[1]
end

function disutility_b_time(sol, discount)
  quadgk(function(t) sol(t)[7]*exp(-discount*t) end, 0, 20)[1]
end

function healthy_time(sol, discount)
  quadgk(function(t) sol(t)[1]*exp(-discount*t) end, 0, 80)[1]
end

function outcome(sol, params)
  cost = params[:costT] + 
         params[:costA]*discounted_a(sol, params[:discount], params[:riskA]) +
         params[:costB]*discounted_b(sol, params[:discount], params[:riskB])
  
  qaly = healthy_time(sol, params[:discount]) +
         (1-params[:disA])*disutility_a_time(sol, params[:discount]) -
         params[:disB]*disutility_b_time(sol, params[:discount])

  Dict(
    :dCost => cost,
    :dQALY => qaly
  )
end

function params_as_dict(df)
  Dict(
    :riskA  => instantaneous_rate(df[1,:vRiskA],5),
    :riskB  => instantaneous_rate(df[1,:vRiskB],5),
    :fatalA => df[1,:vFatalA],
    :rrB    => df[1,:vRR],
    
    :costA  => 10000.0,
    :costB  => 25000.0,
    :costT  => 0.0,
    :disA   => 0.25,  # Permanent disutility for a
    :disB   => 0.1,   # 1-year disutility for b

    :discount => 0.03         # For computing discount
  )
end

function solution(df::DataFrames.DataFrame)

  params = params_as_dict(df)

  pf_simple.params[1] = params[:riskA]
  pf_simple.params[2] = params[:riskB]
  pf_simple.params[3] = params[:fatalA]
  
  prob = ConstantLagDDEProblem(
    pf_simple,
    hh,
    vcat([1.0], zeros(6)),
    [1,5],
    (0.0, 80.0))
    
  sol1=solve(prob, MethodOfSteps(Tsit5()))

  pf_simple.params[1] = pf_simple.params[1]*params[:rrB]
  
  sol2=solve(prob, MethodOfSteps(Tsit5()))
  
  (sol1, sol2)

  #[1,2,3]
end

#### Main Loop
cube = readtable("test-cube.csv")
for i in [1:(size(cube)[1])...]
  println(join([@sprintf "%.f" x for x in solution(cube[i,:])], ", "))
end

