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

c_a  = 10000.0            # Cost of event A
c_b  = 25000.0            # Cost of event B for a year
c_t  = 0.0                # Cost of therapy
d_a  = 0.25               # Permanent disutility for a
d_b  = 0.1                # 1-year disutility for b 
  
disc_rate = 1e-12         # For computing discount

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
  
  if(t>=1 && t <= 11) 
    dy[7] = dy[7] - (r_b*h(t-1)[3])*f_40yr_death_rate_1yr(t)
  end
end
            
pf_simple = DDEParameterizedFunction(simple, [instantaneous_rate(0.1, 5), instantaneous_rate(0.5, 5),  0.05])

hh(t) = zeros(7)

prob = ConstantLagDDEProblem(pf_simple,hh,vcat([1.0], zeros(6)),[1,5],(0.0, 80.0))
#sol=solve(prob, MethodOfSteps(Tsit5(), constrained=true))
sol=solve(prob, MethodOfSteps(Tsit5()))
plot(sol)
