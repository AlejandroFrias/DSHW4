ssh afrias@amazonia.cs.hmc.edu erl -noshell -run ~/courses/DistributedSystems/DSHW4/philosopher main a
ssh afrias@arden.cs.hmc.edu erl -noshell -run ~/courses/DistributedSystems/DSHW4/philosopher main b a@amazonia
ssh afrias@ash.cs.hmc.edu erl -noshell -run ~/courses/DistributedSystems/DSHW4/philosopher main c a@amazonia b@arden
