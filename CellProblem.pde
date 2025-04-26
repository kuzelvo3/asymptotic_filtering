TITLE 'CellProblem'


 {
 	This script solves the cell problem for a manually chosen porosity value.
    We model diffusion (Laplace's equation) in a unit cell with periodic boundary conditions containing a spherical obstacle in the center.
    The results are used for determining the effective diffusion coefficient.
 }
 
 
variables

    u


definitions

{ Parameters }
   eps=0.01			{ small parameter for setting a Dirichlet boundary condition }
   d = 1					{ diffusion coefficient inside the unit cell }
   h=0					{ no sources in the diffusion equation }
 
{ Manual choice of the desired porosity }
   phi=0.55
   
{ Radius of the spherical obstacle based on porosity }
   R=sqrt((1-phi)/pi)
   
   
equations

   u : div(d*grad(u)) + h = 0			{ Laplace's equation }


boundaries

{ Boundary conditions for the unit cell }
  region 1

    start(-0.5,-0.5)
    
    { Definition of periodic boundary conditions for individual edges }
    periodic(x,y+1)   line to (-eps,-0.5)
    value(u)=0 line to (eps,-0.5)			{ manually setting zero Dirichlet condition for uniqueness of the solution }
    periodic(x,y+1)   line to (0.5,-0.5)
    periodic(x-1,y) line to (0.5,0.5)
    
    { No boundary conditions for the rest of the edges, due to the periodicity}
    nobc(u)   line to (eps,0.5)
    value(u)=0 line to (-eps,0.5)			{ reproducing the zero Dirichlet condition for periodicity }
    nobc(u)   line to (-0.5,0.5)
    nobc(u)

    line to close


{ Boundary condition for the spherical obstacle }
  region 2
  		d=0.000001 { almost zero diffusivity inside the sphere simulating zero diffusion }
        
        start(R,0)
        
        { Natural boundary condition based on the normal vector to the obstacle surface }
		natural(u) = -x/sqrt(x^2+y^2)
        
        { Definition of the spherical boundary }
    	arc(center = 0,0) to (-R,0)
        arc(center = 0,0) to (0,-R)
        arc(center = 0,0) to close


monitors

    grid(x,y)
    contour(u)


plots

    grid(x,y)

	{ Export of the results into a table for further calculations }
    contour(u) export format "{#x,#y,#1}," file="CellProblem_Results\CellProblemTable_550.txt"


end