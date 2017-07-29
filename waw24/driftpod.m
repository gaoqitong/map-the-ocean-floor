function [dvals,rvals] = driftpod(d0, r0, finaltime, vcurrent, vy0, vx0)

% Greater depth = positive

if (nargin<1)
    error('Must provide initial depth value')
end
if (nargin<2)
    r0=0;
end
if (nargin<3)
    finaltime = 10;
end
if (nargin<4)
    vcurrent=0;
end
if (nargin<6)
    vy0=0;
    vx0=0;
end

% Parameters
g = 9.8; % m/s^2
M = 65; % kg
r = 0.25; % m
h = 1; % m
Vol = h*pi*(r./2)^2; % m^3
Aside = 1*0.25; % m^2
Abot = pi*0.25^2; % m^2
dwater = 1027; % kg/m^3
Cy = 0.82;
Cx = 1;
dt = 0.1; % s
times = [0:0.01:dt dt:dt:finaltime];

% Initial Conditions
y=zeros(1,length(times));
x=y;
vy=y;
vx=y;
ay=y;
ax=y;

y(1)=d0;
x(1)=r0;
vy(1)=vy0;
vx(1)=vx0;
ay(1)= (M*g-dwater*Vol*g)/M;
ax(1)= -1/2*Aside*Cx*dwater*(vx(1)-vcurrent).^2;

% Euler's method loop
for k=1:(length(times)-1);
    % Y direction
    %Fbuoyancy=dwater*Vol*g; % Down is positive
    %Fdrag = -CAdv^2/2
    vy(k+1) = vy(k)+ay(k)*dt;
    y(k+1) = y(k)+vy(k)*dt;
    ay(k+1) = (-1/2*Cy*Abot*dwater*(vy(k+1)).^2+M*g-dwater*Vol*g)/M;

    % X direction
    vx(k+1) = vx(k)+ax(k)*dt;
    x(k+1) = x(k)+vx(k)*dt;
    if vcurrent>vx(k+1)
        ax(k+1) = (1/2*Cx*Aside*dwater*(vcurrent-vx(k+1)).^2)/M;
    end
    if vcurrent<vx(k+1)
        ax(k+1) = -(1/2*Cx*Aside*dwater*(vcurrent-vx(k+1)).^2)/M;
    end
end

dvals = y;
rvals = x;



