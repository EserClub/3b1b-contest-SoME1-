from typing import Text
from numpy import maximum, put_along_axis
from big_ol_pile_of_manim_imports import *
import itertools as it
import array as arr
from manimlib.mobject.numbers import DecimalNumber
from manimlib.animation.creation import Write, DrawBorderThenFill, ShowCreation
from manimlib.animation.transform import Transform
from manimlib.animation.update import UpdateFromAlphaFunc
from manimlib.constants import *
from manimlib.mobject.functions import ParametricFunction
from manimlib.mobject.geometry import Line
from manimlib.mobject.geometry import Rectangle
from manimlib.mobject.geometry import RegularPolygon
from manimlib.mobject.number_line import NumberLine
from manimlib.mobject.svg.tex_mobject import TexMobject
from manimlib.mobject.svg.tex_mobject import TextMobject
from manimlib.mobject.types.vectorized_mobject import VGroup
from manimlib.mobject.types.vectorized_mobject import VectorizedPoint
from manimlib.scene.scene import Scene
from manimlib.utils.bezier import interpolate
from manimlib.utils.color import color_gradient
from manimlib.utils.color import invert_color
from manimlib.utils.space_ops import angle_of_vector
from NumberCreature.NumberCreature import*

class Costraints(GraphScene,ThreeDScene):
    CONFIG = {
        "rows":8,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
    }
    def setup(self):
     ThreeDScene.setup(self)
     GraphScene.setup(self)

    def construct(self):
     self.move_camera(phi=70*DEGREES, theta=20*DEGREES,frame_center=(-1.5,4,0))
     self.begin_ambient_camera_rotation(rate=0.01)
     self.setup_axes(animate=False)
     graph=self.get_graph(lambda x: np.cos(2*np.cos(x))*np.sin(2*np.sin(x))+2, x_min=0, x_max=TAU, color=RED)  
     line2 = self.get_vertical_line_to_graph(PI/2, graph, DashedLine, color=YELLOW)
     tma=TexMobject(r"\theta_{\text{max}}").set_color(GREEN).scale(.6).next_to(line2,DOWN,buff=SMALL_BUFF)
     hl = DashedLine( self.coords_to_point(PI/2,np.sin(2)+2), self.coords_to_point(0,np.sin(2)+2), color=YELLOW)
     tt=TexMobject(r"f(\theta_{\text{max}})").set_color(RED).scale(.6).next_to(hl,LEFT,buff=SMALL_BUFF)
     k=Line(self.coords_to_point(-1,-10), self.coords_to_point(-1,10), color=GREY)
     tt[2:6].set_color(GREEN)
     group = VGroup(k,tt,tma,line2,hl,graph, self.x_axis, self.y_axis, self.axes)
     group.scale_in_place(.7)
     group.move_to(2*RIGHT+3*UP)
     self.add_fixed_in_frame_mobjects(group)
     axes = ThreeDAxes()
     grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
     grilla2=grilla.copy().next_to(grilla,UP,buff=0)
     grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
     grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
     grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
     grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
     grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
     self.add(axes,k)
     surface=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(u)*np.sin(v)+2
            ]),v_min=-4,v_max=4,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63),gloss=.9).fade(.7).set_color_by_gradient(GREEN,BLUE)
     curve=ParametricFunction(
                lambda u : np.array([
                2*np.cos(u),
                2*np.sin(u),
                0
            ]),color=RED,t_min=0,t_max=TAU,
            )
     curve2=ParametricFunction(
                lambda u : np.array([
                2*np.cos(u),
                2*np.sin(u),
                np.cos(2*np.cos(u))*np.sin(2*np.sin(u))+2
            ]),color=RED,t_min=0,t_max=TAU,
            )     
     f=TexMobject(r"z=f(x,y)").next_to(surface,Z_AXIS).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS)  
     self.add(surface,f)
     def arrow(u=0):
        return Arrow(np.array([
                2*np.cos(u),
                2*np.sin(u),
                0
            ]),np.array([
                2*np.cos(u),
                2*np.sin(u),
                np.cos(2*np.cos(u))*np.sin(2*np.sin(u))+2
            ]), buff=0).set_color(YELLOW)
     k=arrow()     ###### updating the curve to follow the area################
     def update_arrow(k, alpha):
            ddx = interpolate(0, TAU, alpha)
            k_k = arrow(ddx)
            k.become(k_k)

     def line(u=0):
        return DashedLine((0,0,0),(2*np.cos(u),2*np.sin(u),0)).set_color(YELLOW)
     l=line()     ###### updating the curve to follow the area################
     def update_line(l,alpha):
         dl = interpolate(0, TAU, alpha)
         ll = line(dl)
         l.become(ll) 
     def ano(dtheta=TAU):
         return Sector(inner_radius=0,outer_radius=.5,angle=dtheta,color=PURPLE,stroke_width=0).set_fill(GREEN, opacity=.8)
     cc=ano()
     def update_theta(cc,alpha):
         dtheta=interpolate(0,TAU,alpha)
         dt=ano(dtheta)
         cc.become(dt)           
     theta=TexMobject(r"\theta").rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).set_color(GREEN).scale(.6)
     theta.add_updater(lambda m: m.move_to(l.get_center()+.5*UP))
     theta.shift(.1*UP)
     self.play(GrowArrow(k),ShowCreation(l),FadeIn(theta))
     self.wait()
     self.play(ShowCreation(graph,run_time=7),ShowCreation(curve,run_time=7),ShowCreation(curve2,run_time=7),UpdateFromAlphaFunc(k,update_arrow,run_time=7),UpdateFromAlphaFunc(l,update_line,run_time=7),UpdateFromAlphaFunc(cc,update_theta,run_time=7))
     self.wait(1)
     self.play(FadeOut(f),FadeOut(surface),FadeOut(k))
     self.wait()
     ft=TexMobject(r"z=f(\theta)").next_to(curve2,UP).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).set_color(RED)
     self.play(FadeIn(ft))
     self.wait(1)
     def update_theta2(cc,alpha):
         dtheta=interpolate(TAU,PI/2,alpha)
         dt=ano(dtheta)
         cc.become(dt)  
     def update_line2(l,alpha):
         dl = interpolate(TAU, PI/2, alpha)
         ll = line(dl)
         l.become(ll)  
     self.play(UpdateFromAlphaFunc(l,update_line2,run_time=2),UpdateFromAlphaFunc(cc,update_theta2,run_time=2))
     lz=DashedLine((0,2,0),(0,2,2.9093))
     zl=DashedLine((0,2,2.9093),(0,0,2.9093))
     self.play(ShowCreation(lz),ShowCreation(line2))
     self.play(ShowCreation(zl),ShowCreation(hl))
     z=TexMobject(r"z=f(\theta_{\text{max}})").set_color(RED).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).scale(.6).move_to((0,-1,2.9093))
     z[4:8].set_color(GREEN)
     tm=TexMobject(r"\theta_\text{max}").set_color(GREEN).scale(.6)
     theta.clear_updaters()
     tm.move_to(.7*RIGHT+1*UP).rotate(PI/2,axis=Z_AXIS).rotate(PI/2,axis=Y_AXIS)
     self.play(Write(z),Transform(theta,tm),Write(tt),Write(tma))
     self.wait(1)

class PartialD(ThreeDScene):
    CONFIG = {
        "rows":8,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
    }
    def construct(self):
     self.move_camera(phi=70*DEGREES, theta=20*DEGREES)
     self.begin_ambient_camera_rotation(rate=0.01)
     axes = ThreeDAxes()
     grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
     grilla2=grilla.copy().next_to(grilla,UP,buff=0)
     grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
     grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
     grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
     grilla6=grilla.copy().next_to(grilla,RIGHT+DOWN,buff=0)
     grilla7=NumberPlane(width=24,height=4,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke).next_to(grilla,RIGHT,buff=0)
     grilla8=grilla.copy().next_to(grilla,LEFT,buff=0)
     self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8)
     surface=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(u)*np.sin(v)+2
            ]),v_min=-4,v_max=4,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color_by_gradient(GREEN,BLUE)
     curve=ParametricFunction(
                lambda u : np.array([
                1,
                u,
                0
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )
     curve2=ParametricFunction(
                lambda u : np.array([
                1,
                u,
                np.cos(1)*np.sin(u)+2
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )     
     plane=ParametricSurface(
                lambda u,v : np.array([
                1,
                u,
                v
            ]),v_min=-1,v_max=3,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color(WHITE)
     self.add(surface)
     self.wait(4)
     #self.play(ShowCreation(plane))
     #self.wait(1)
     def arrow(u=-2.5):
        return DashedLine(np.array([
                1,
                u,
                0
            ]),np.array([
                1,
                u,
                np.cos(1)*np.sin(u)+2
            ]), buff=0).set_color(YELLOW)
     k=arrow()     ###### updating the curve to follow the area################
     def update_arrow(k, alpha):
            ddx = interpolate(-2.5, 3.5, alpha)
            k_k = arrow(ddx)
            k.become(k_k)
     self.play(ShowCreation(k))
     self.play(UpdateFromAlphaFunc(k,update_arrow),ShowCreation(curve),ShowCreation(curve2), run_time=4,rate_func=linear)
     self.play(FadeOut(k))
     self.wait(1)
     def derivative(v=-2.5):#fucking tangent line
        return ParametricFunction(
                lambda u : np.array([
                1,
                u,
                np.arctan(np.cos(1)*np.cos(v))*(u-v)+np.cos(1)*np.sin(v)+2
            ]),color=BLUE,t_min=v-1,t_max=v+1,
            )
     d=derivative()     ###### updating the curve to follow the area################
     def update_derivative(d, alpha):
            ddx = interpolate(-2.5, PI/2, alpha)
            dd = derivative(ddx)
            d.become(dd)
     par=TextMobject(r"Partial Derivative").set_color(PINK).scale(1.2)
     self.add_fixed_in_frame_mobjects(par)
     par.to_corner(UL)
     t=TexMobject(r"\frac{\partial f}{\partial x}").set_color(BLUE).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).scale(.7).move_to(d.get_center()+Z_AXIS*.7)
     self.play(ShowCreation(d),Write(t))
     t.add_updater(lambda m: m.move_to(d.get_center()+Z_AXIS*.7))
     self.play(UpdateFromAlphaFunc(d,update_derivative,run_time=3.5,rate_func=linear))
     self.play(FadeInFromDown(par))
     self.wait(.5)
     self.play(FadeOutAndShift(par,UP))
     dfiga=ParametricFunction(
                lambda u : np.array([
                1,
                u,
                np.cos(1)*np.sin(PI/2)+2
            ]),color=GREEN,t_min=PI/2-2,t_max=PI/2+2,
            )
     t2=TexMobject(r"=0").set_color(BLUE).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).scale(.7).next_to(t,UP,buff=SMALL_BUFF)
     self.play(ShowCreation(t2[0]))
     self.play(Write(t2[1]))
     self.play(Transform(d,dfiga),run_time=1,rate_func=there_and_back)
     self.wait(2)
     self.move_camera(phi=70*DEGREES, theta=110*DEGREES)
     T=VGroup(t,t2)
     self.play(FadeOut(T))
     ycurve=ParametricFunction(
                lambda u : np.array([
                u,
                1,
                0
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )
     ycurve2=ParametricFunction(
                lambda u : np.array([
                u,
                1,
                np.cos(u)*np.sin(1)+2
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )     
     yplane=ParametricSurface(
                lambda u,v : np.array([
                u,
                1,
                v
            ]),v_min=-1,v_max=3,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color(WHITE)
     #self.play(ShowCreation(plane))
     #self.wait(1)
     def yarrow(u=-2.5):
        return DashedLine(np.array([
                u,
                1,
                0
            ]),np.array([
                u,
                1,
                np.cos(u)*np.sin(1)+2
            ]), buff=0).set_color(YELLOW)
     yk=yarrow()     ###### updating the curve to follow the area################
     def yupdate_arrow(yk, alpha):
            yddx = interpolate(-2.5, 3.5, alpha)
            yk_k = yarrow(yddx)
            yk.become(yk_k)
     self.play(ShowCreation(yk))
     self.play(UpdateFromAlphaFunc(yk,yupdate_arrow),ShowCreation(ycurve),ShowCreation(ycurve2), run_time=4,rate_func=linear)
     self.play(FadeOut(yk))
     self.wait(1)
     #self.play(ShowCreation(yplane))
     #self.wait(1)
     def yderivative(v=3.5):#fucking tangent line
        return ParametricFunction(
                lambda u : np.array([
                u,
                1,
                np.arctan(-np.sin(v)*np.sin(1))*(u-v)+np.cos(v)*np.sin(1)+2
            ]),color=YELLOW,t_min=v-1,t_max=v+1,
            )
     yd=yderivative()     ###### updating the curve to follow the area################
     def yupdate_derivative(yd, alpha):
            ddx = interpolate(3.5, 0, alpha)
            ydd = yderivative(ddx)
            yd.become(ydd)
     yt=TexMobject(r"\frac{\partial f}{\partial y}").set_color(YELLOW).rotate(PI/2,axis=X_AXIS).rotate(PI,axis=Z_AXIS).scale(.7).move_to(yd.get_center()+Z_AXIS*.7)
     self.play(ShowCreation(yd),Write(yt))
     yt.add_updater(lambda m:  m.move_to(yd.get_center()+Z_AXIS*.7))
     self.play(UpdateFromAlphaFunc(yd,yupdate_derivative,run_time=3.5,rate_func=linear))
     self.wait(1)
     ydfiga=ParametricFunction(
                lambda u : np.array([
                u,
                1,
                np.cos(0)*np.sin(1)+2
            ]),color=GREEN,t_min=-2,t_max=+2,
            )
     yt2=TexMobject(r"=0").set_color(YELLOW).rotate(PI/2,axis=X_AXIS).rotate(PI,axis=Z_AXIS).scale(.7).next_to(yt,LEFT,buff=SMALL_BUFF)
     self.play(ShowCreation(yt2[0]))
     self.play(Write(yt2[1]))
     self.play(Transform(yd,ydfiga),run_time=1,rate_func=there_and_back)
     self.wait(1)
     def ymobasta(v=1):#fucking tangent line
        return ParametricFunction(
                lambda u : np.array([
                u,
                v,
                np.cos(u)*np.sin(v)+2
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )
     ymb=ymobasta()     ###### updating the curve to follow the area################
     def yupdate_basta(ymb, alpha):
            ddx = interpolate(1, PI/2, alpha)
            ymbb = ymobasta(ddx)
            ymb.become(ymbb)

     def xmobasta(v=1):#fucking tangent line
        return ParametricFunction(
                lambda u : np.array([
                v,
                u,
                np.cos(v)*np.sin(u)+2
            ]),color=RED,t_min=-2.5,t_max=3.5,
            )
     xmb=xmobasta()     ###### updating the curve to follow the area################
     def xupdate_basta(xmb, alpha):
            ddx = interpolate(1, 0, alpha)
            xmbb = xmobasta(ddx)
            xmb.become(xmbb)
     self.move_camera(phi=70*DEGREES, theta=60*DEGREES)
     self.play(FadeOut(d),FadeOut(yd),FadeOut(curve),FadeOut(ycurve),FadeOut(yt2),FadeOut(yt))
     self.play(ReplacementTransform(ycurve2,ymb),ReplacementTransform(curve2,xmb))
     self.play(UpdateFromAlphaFunc(ymb,yupdate_basta,run_time=2.5,rate_func=linear),UpdateFromAlphaFunc(xmb,xupdate_basta,run_time=2.5,rate_func=linear))
     self.wait(1)
     d.move_to((0,PI/2,3))
     yd.move_to((0,PI/2,3))
     dashed=DashedLine((0,PI/2,0),(0,PI/2,3))
     self.play(ShowCreation(dashed))
     self.play(ShowCreation(d),ShowCreation(yd))
     self.wait(1)
     nabla=TexMobject(r"\vec{\nabla}f=\left(\frac{\partial f}{\partial x},\frac{\partial f}{\partial y}\right)=(0,0)")
     nabla[19].set_color(BLUE)
     nabla[21].set_color(YELLOW)
     nabla[5:10].set_color(BLUE)
     nabla[11:16].set_color(YELLOW)
     r=SurroundingRectangle(nabla[0:3]).set_color(PINK)
     gradient=TextMobject(r"Gradient").scale(.7).set_color(PINK)
     self.add_fixed_in_frame_mobjects(nabla,gradient,r)
     nabla.to_corner(UL)
     self.play(Write(nabla[0:3]))
     r.move_to(nabla[0:3])
     self.wait()
     gradient.next_to(nabla[0:3],DOWN)
     self.play(FadeInFromDown(gradient))
     self.play(ShowCreationThenDestruction(r))
     self.play(FadeOutAndShiftDown(gradient))
     self.play(ShowCreation(nabla[3]))
     self.play(Write(nabla[4:23]))
     self.wait(1)
     self.play(FadeOut(xmb),FadeOut(ymb))
     #self.move_camera(phi=90*DEGREES, theta=0*DEGREES,frame_center=(0,.1,1))
     #self.stop_ambient_camera_rotation()
     #self.play(FadeOut(plane))
     tan=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                3
            ]),v_min=-2+PI/2,v_max=2+PI/2,u_min=-2,u_max=2,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color(WHITE)
     self.play(ShowCreation(tan))
     self.wait(1)
     self.play(FadeOut(d),FadeOut(yd))
     parab=ParametricSurface(#POLINOMIO TAYLOR 2o GRADO 
                lambda u,v : np.array([
                u,
                v,
                3+.5*(-u**2-(v-PI/2)**2)
            ]),v_min=-2+PI/2,v_max=2+PI/2,u_min=-2,u_max=2,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.65).set_color(PURPLE)
     self.wait(3)
     self.play(Transform(tan,parab),run_time=3)
     det=TexMobject(r"f(x,y)\approx \text{Osculating Paraboloid}").set_color(PURPLE).scale(1.5)
     self.add_fixed_in_frame_mobjects(det)
     det.to_corner(DL)
     self.play(Write(det))
     self.wait(3)

class Taylor(GraphScene):
    CONFIG = {
        "rows":20,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
     }
    def construct(self):
        grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        self.setup_axes(animate=False)       
        f=self.get_graph(lambda x: 0.1*((x-6)**3+(x-6)**2-6*(x-6))+2, x_min=2,x_max=9).set_color(RED)
        fapprox=TexMobject(r"f(x)\approx f(x_0)+f'(x_0)(x-x_0)").to_corner(UL)
        fapprox[0].set_color(RED)
        fapprox[5].set_color(RED)
        fapprox[11:13].set_color(RED)
        fapprox[7:9].set_color(GREEN)
        fapprox[14:16].set_color(GREEN)
        fapprox[20:22].set_color(GREEN)
        fapprox2=TexMobject(r"f(x)\approx f(x_0)+\frac{f''(x_0)}{2}(x-x_0)^2").to_corner(UL,buff=MED_SMALL_BUFF)
        fapprox2[0].set_color(RED)
        fapprox2[5].set_color(RED)
        fapprox2[11:14].set_color(RED)
        fapprox2[7:9].set_color(GREEN)
        fapprox2[15:17].set_color(GREEN)
        fapprox2[23:25].set_color(GREEN)
        def derivative(u=2.5):
             return self.get_graph(lambda x: 0.1*(3*((u)-6)**2+2*((u)-6)-6)*(x-(u))+0.1*(((u)-6)**3+((u)-6)**2-6*((u)-6))+2, x_min=u-1,x_max=u+1).set_color(BLUE)
        k=derivative()
        def update_derivative(k,alpha):
         uu = interpolate(2.5, 17/3-np.sqrt(19)/3, alpha)
         kk = derivative(uu)
         k.become(kk)
        def vl(u=2.5):
            return self.get_vertical_line_to_graph(u,f,line_class=DashedLine,color=YELLOW)
        v=vl()
        x0=TexMobject(r"x_0").set_color(GREEN).next_to(v,DOWN)
        x0.add_updater(lambda m: m.next_to(v,DOWN))
        def update_vl(v,alpha):
            uu=interpolate(2.5, 17/3-np.sqrt(19)/3, alpha)
            vv=vl(uu)
            v.become(vv)
        self.add(grilla,self.axes,self.x_axis,self.y_axis,f,fapprox,x0,v)
        self.play(UpdateFromAlphaFunc(v,update_vl),UpdateFromAlphaFunc(k,update_derivative), run_time=4)
        self.wait()
        def derivative2(u=17/3-np.sqrt(19)/3):
            return self.get_graph(lambda x: 0.1*(6*((u)-6)+2)*(x-(u))**2/2+0.1*(3*((u)-6)**2+2*((u)-6)-6)*(x-(u))+0.1*(((u)-6)**3+((u)-6)**2-6*((u)-6))+2, x_min=u-1,x_max=u+1).set_color(BLUE)
        k2=derivative2()
        def derivative3(u=17/3-np.sqrt(19)/3):
            return self.get_graph(lambda x: 0.1*(6*((u)-6)+2)*(x-(u))**2/2+0.1*(3*((u)-6)**2+2*((u)-6)-6)*(x-(u))+0.1*(((u)-6)**3+((u)-6)**2-6*((u)-6))+2, x_min=u-2,x_max=u+2).set_color(ORANGE)
        k3=derivative3()
        self.play(ReplacementTransform(k,k2),ReplacementTransform(fapprox,fapprox2),run_time=2)
        self.wait()
        r=SurroundingRectangle(fapprox2[11:20]).set_color(YELLOW)
        self.play(ShowCreationThenDestruction(r))
        par=TexMobject(r"y=a(x-x_0)^2+y_0").scale(.7).next_to(k2,UP)
        par[2].set_color(ORANGE)
        par[6:8].set_color(GREEN)
        self.play(TransformFromCopy(fapprox2[11:20],par[2]),FadeInFrom(par[0:2],RIGHT),FadeInFrom(par[3:13],LEFT))
        def update_par(k2,alpha):
            uu=interpolate(17/3-np.sqrt(19)/3,17/3+np.sqrt(19)/3,alpha)
            k2k2=derivative2(uu)
            k2.become(k2k2)
        par.add_updater(lambda m: m.next_to(k2,UP))
        def vl2(u2=17/3-np.sqrt(19)/3):
            return self.get_vertical_line_to_graph(u2,f,line_class=DashedLine,color=YELLOW)
        v2=vl2()
        x02=TexMobject(r"x_0").set_color(GREEN).next_to(v2,DOWN)
        x02.add_updater(lambda m: m.next_to(v2,DOWN))
        def update_vl2(v2,alpha):
            uu=interpolate(17/3-np.sqrt(19)/3, 17/3+np.sqrt(19)/3, alpha)
            vv2=vl2(uu)
            v2.become(vv2) 
        self.add(par,x02)
        self.play(Transform(k2,k3),rate_func=there_and_back)
        self.wait(1)
        self.play(UpdateFromAlphaFunc(v2,update_vl2),UpdateFromAlphaFunc(k2,update_par), run_time=4)
        self.wait()
        dn=TexMobject(r"f''(x_0)<0\Rightarrow x_{\text{max}}").scale(.7).move_to(self.coords_to_point(4,3))
        xmax=TexMobject(r"x_{\text{max}}").set_color(BLUE).move_to(x0)
        xmin=TexMobject(r"x_{\text{min}}").set_color(PURPLE).move_to(x02)
        dn[0:3].set_color(RED)
        dn[4:6].set_color(GREEN)
        dn[10:14].set_color(BLUE)
        dN=TexMobject(r"f''(x_0)>0\Rightarrow x_{\text{min}}").scale(.7).move_to(self.coords_to_point(7.1,1.2))
        dN[0:3].set_color(RED)
        dN[4:6].set_color(GREEN)
        dN[10:14].set_color(PURPLE)
        self.play(Write(dn[0:9]))
        self.play(GrowFromEdge(dn[9],LEFT))
        self.play(GrowFromEdge(dn[10:14],LEFT))
        self.wait()
        self.play(Transform(x0,xmax))
        self.wait()
        self.play(Write(dN[0:9]))
        self.play(GrowFromEdge(dN[9],LEFT))
        self.play(GrowFromEdge(dN[10:14],LEFT))
        self.wait()
        self.play(Transform(x02,xmin))
        self.play(FadeOut(dn),FadeOut(dN),Uncreate(k2),FadeOut(par))
        self.wait()

class Derivative(GraphScene):
      CONFIG = {
        "rows":20,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
     }
      def construct(self):
        grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        self.setup_axes(animate=False)       
        f=self.get_graph(lambda x: 0.1*((x-6)**3+(x-6)**2-6*(x-6))+2, x_min=2,x_max=9).set_color(RED)
        self.add(grilla,self.axes,self.x_axis,self.y_axis,f)
        d=VGroup(*list(Line(self.coords_to_point(-2*(2+i/4),0.1*(3*((2+i/4)-6)**2+2*((2+i/4)-6)-6)*(-2*(2+i/4)-(2+i/4))+0.1*(((2+i/4)-6)**3+((2+i/4)-6)**2-6*((2+i/4)-6))+2),self.coords_to_point(2*(2+i/4),0.1*(3*((2+i/4)-6)**2+2*((2+i/4)-6)-6)*(2*(2+i/4)-(2+i/4))+0.1*(((2+i/4)-6)**3+((2+i/4)-6)**2-6*((2+i/4)-6))+2)) for i in range(1,16))).set_color(BLUE)
        #x=Line(self.coords_to_point(-1,0),self.coords_to_point(9,0)).set_color(GREY)
        #fd=TexMobject(r"m=f'(x)>0").to_corner(UL)
        #fdd=TexMobject(r"f(x)\,\,\text{grows}").next_to(fd,DOWN)
        #self.play(FadeIn(fd),FadeIn(fdd))
        for i in range(1,8):
         #s=Arc(radius=.5, start_angle=x.get_angle(),angle=d[i].get_angle(),arc_center=self.coords_to_point(-(0.1*((2+i/3-6)**3+(2+i/3-6)**2-6*(2+i/3-6))+2)/(0.1*(3*(2+i/3-6)**2+2))+2+i/3,0)).set_fill(color=YELLOW,opacity=.5)
         #s.move_to(self.coords_to_point(-(0.1*((2+i/3-6)**3+(2+i/3-6)**2-6*(2+i/3-6))+2)/(0.1*(3*(2+i/3-6)**2+2))+2+i/3,0))
         self.play(ShowCreation(d[i]))
        # self.play(ShowCreation(s))
        self.play(FadeOut(d[1:8]))
        self.wait(1)
        d[8:16].set_color(PURPLE)
        for i in range(8,14):
            self.play(ShowCreation(d[i]))
        self.play(FadeOut(d[8:14]))
        def derivative(u=2.5):
            if 0.1*(3*((u)-6)**2+2*((u)-6)-6)>0:
             return self.get_graph(lambda x: 0.1*(3*((u)-6)**2+2*((u)-6)-6)*(x-(u))+0.1*(((u)-6)**3+((u)-6)**2-6*((u)-6))+2, x_min=u-2,x_max=u+2).set_color(BLUE)
            elif  0.1*(3*((u)-6)**2+2*((u)-6)-6)<=0:
             return self.get_graph(lambda x: 0.1*(3*((u)-6)**2+2*((u)-6)-6)*(x-(u))+0.1*(((u)-6)**3+((u)-6)**2-6*((u)-6))+2, x_min=u-2,x_max=u+2).set_color(PURPLE)

        k=derivative()
        def update_derivative(k,alpha):
         uu = interpolate(2.5, 17/3-np.sqrt(19)/3, alpha)
         kk = derivative(uu)
         k.become(kk)
        self.play(ShowCreation(k))
        self.play(UpdateFromAlphaFunc(k,update_derivative), run_time=4)
        l=Line((1,0,0),(9,0,0)).set_color(YELLOW).move_to(k)
        self.play(Transform(k,l),run_time=1, rate_func=there_and_back)
        def update_derivative2(k,alpha):
         uu = interpolate(17/3-np.sqrt(19)/3, 17/3+np.sqrt(19)/3, alpha)
         kk = derivative(uu)
         k.become(kk)
        self.wait(1)
        self.play(UpdateFromAlphaFunc(k,update_derivative2), run_time=4)
        l2=Line((1,0,0),(9,0,0)).set_color(YELLOW).move_to(k)
        self.play(Transform(k,l2),run_time=1, rate_func=there_and_back)
        self.play(FadeOut(k))
        opt=TextMobject(r"OPTIMIZATION").scale(2).to_edge(UP,buff=SMALL_BUFF).set_color_by_gradient(BLUE,PURPLE)
        self.play(FadeInFromDown(opt))
        self.wait(1)
        xM=self.get_vertical_line_to_graph(17/3-np.sqrt(19)/3,f,line_class=DashedLine).set_color(YELLOW)
        xm=self.get_vertical_line_to_graph(17/3+np.sqrt(19)/3,f,line_class=DashedLine).set_color(YELLOW)
        Mx=TexMobject(r"x_{\text{max}}").next_to(xM,DOWN).set_color(BLUE)
        mx=TexMobject(r"x_{\text{min}}").next_to(xm,DOWN).set_color(PURPLE)
        self.play(ShowCreation(xM),ShowCreation(xm))
        self.play(Write(Mx),Write(mx))
        self.wait(1)
        g=VGroup(self.x_axis,self.y_axis,self.axes)
        mMx=TexMobject(r"x_{\text{max}}").scale(2).set_color(BLUE).move_to((2,2,0))
        mmx=TexMobject(r"x_{\text{min}}").scale(2).next_to(mMx,DOWN,buff=4).set_color(PURPLE)
        self.play(FadeOut(opt),FadeOut(g),Transform(Mx,mMx),Transform(mx,mmx),Uncreate(xm),Uncreate(f),Uncreate(xM),run_time=1.5)
        equ=TexMobject(r"f'(x)=0\Rightarrow").scale(2).to_edge(LEFT,buff=1)
        self.play(Write(equ[0:5]))
        self.play(ShowCreation(equ[5]))
        self.play(Write(equ[6]))
        self.play(GrowFromEdge(equ[7],LEFT))
        ar1=Arrow(equ[7],mMx[0]).set_color(YELLOW)
        ar2=Arrow(equ[7],mmx[0]).set_color(YELLOW)
        self.play(GrowArrow(ar1),GrowArrow(ar2))
        self.wait(1)
        second=TextMobject(r"SECOND DERIVATIVE").scale(2).to_edge(UP).set_color_by_gradient(BLUE,PURPLE)
        self.play(FadeInFromLarge(second,.1))
        self.wait(2)
        gigi=VGroup(Mx,mx,ar1,ar2,equ,second)
        self.play(FadeOutAndShiftDown(gigi))
        #first_order=VGroup(*list(Line(self.coords_to_point(-.3+(2+i/10),0.1*(3*((2+i/10)-6)**2+2*((2+i/10)-6)-6)*(-.3+(2+i/10)-(2+i/10))+0.1*(((2+i/10)-6)**3+((2+i/10)-6)**2-6*((2+i/10)-6))+2),self.coords_to_point(.3+(2+i/10),0.1*(3*((2+i/10)-6)**2+2*((2+i/10)-6)-6)*(.3+(2+i/10)-(2+i/10))+0.1*(((2+i/10)-6)**3+((2+i/10)-6)**2-6*((2+i/10)-6))+2)) for i in range(0,70))).set_color(RED)
        #self.wait(1)
        #self.play(ShowCreation(first_order))
        #ma c'è un problema, se non abbiamo un disegno che ci aiuti, quando calcolo la derivata e trovo i punti per cui è uguale a 0, come 
        #faccio a distinguerli e a capire chi è il punto di massimo e chi è il punto di minimo?
        #per questo ci aiuta la derivata seconda
        #potremmo cioè effettuare un'altra volta la derivata e calcolare la funzione in quei punti trovati,
        #se in quel punto è positiva è minimo blabla
        #perché? beh perché se la derivata prima permette di approssimare la funzione a un polinomio di primo grado (una retta tangente appunto)
        #la derivata seconda ci farà approssimare a un polinomio di secondo grado. (una parabola)
        #più nello specifico, n base al segnodi f'' capirò se approssimare f a una parabola che va verso l'alto o verso il basso
        #da lì spieghi il concetto

#sebbene questo metodo non è molto usato in analisi 1, in analisi 2 divetna fondamentle. estendiamo infatti il concetto
#(parla di derivate parziali collegandoti alla prossima scena, per poi parlare del paraboloide osculatore)

#suddividi il video in ottimizzazione libera e vincolata (suddividi in rettangoli, funzioni e cerchi (come nel video scorso)) e trova l'esempio pratico da cui partire
class Introduzione(GraphScene):
    CONFIG = {
        "rows":20,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
     }
    def construct(self):
        #grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        #self.add(grilla)
        self.setup_axes(animate=True)
        def line(m=.5):
         return self.get_graph(lambda x: m*x-5*m+2).set_color(RED)
        funz=self.get_graph(lambda x: -3/128*x**3+21/64*x**2-13/16*x+1,x_min=0,color=GREEN).set_stroke(width=6)
        f=line()
        f.save_state()
        self.play(ShowCreation(f),run_time=2, rate_func=linear)
        def update_line(f,alpha):
         mm = interpolate(.5, 4, alpha)
         ff = line(mm)
         f.become(ff)
        self.wait()
        scritta=TexMobject(r"y=mx+q").scale(2).to_corner(UL)
        scritta[2].set_color(MAROON)   
        scritta[5].set_color(BLUE)     
        self.play(Write(scritta))
        self.wait(1)
        self.play(scritta[2].scale,1.5)
        self.play(scritta[2].scale,0.7)
        self.wait(1)
        M=TexMobject(r"m=").set_color(MAROON).scale(1.2)
        time=ValueTracker(.5)
        nM=DecimalNumber(.5).set_color(MAROON)
        nM.scale(2).add_updater(lambda m: m.set_value(time.get_value())).to_edge(RIGHT)
        M.next_to(nM,LEFT)
        self.play(FadeInFromDown(M),FadeInFromDown(nM))
        self.play(UpdateFromAlphaFunc(f,update_line), time.set_value,4,run_time=3)
        def update_line2(f,alpha):
         mm = interpolate(4, -2, alpha)
         ff = line(mm)
         f.become(ff)
        self.play(UpdateFromAlphaFunc(f,update_line2),time.set_value,-2,run_time=3)
        def update_line3(f,alpha):
         mm = interpolate(-2, 0, alpha)
         ff = line(mm)
         f.become(ff)
        self.play(UpdateFromAlphaFunc(f,update_line3),time.set_value,0,run_time=1)
        self.wait()
        self.play(Restore(f),time.set_value,.5)
        self.play(FadeOutAndShiftDown(nM),FadeOutAndShiftDown(M))
        x1=self.get_vertical_line_to_graph(4,f,line_class=DashedLine).set_color(YELLOW)
        tx1=TexMobject(r"x_1").next_to(x1,DOWN).set_color(ORANGE)
        x2=self.get_vertical_line_to_graph(8,f,line_class=DashedLine).set_color(YELLOW)
        tx2=TexMobject(r"x_2").next_to(x2,DOWN).set_color(PURPLE)
        y1=DashedLine(self.coords_to_point(0,4*.5-5*.5+2),self.coords_to_point(4,4*.5-5*.5+2)).set_color(YELLOW)
        ty1=TexMobject(r"y_1").next_to(y1,LEFT).set_color(ORANGE)
        y2=DashedLine(self.coords_to_point(0,.5*8-5*.5+2),self.coords_to_point(8,.5*8-5*.5+2)).set_color(YELLOW)
        ty2=TexMobject(r"y_2").next_to(y2,LEFT).set_color(PURPLE)
        self.play(ShowCreation(x1),ShowCreation(x2))
        self.play(ShowCreation(y1),ShowCreation(y2))
        self.play(ShowCreation(tx1),ShowCreation(tx2))
        self.play(ShowCreation(ty1),ShowCreation(ty2))
        self.wait(1)
        yy2=y2.copy()
        y2meno1=Line(self.coords_to_point(4,4*.5-5*.5+2),self.coords_to_point(8,.5*4-5*.5+2)).set_color(RED)
        xx1=x1.copy()
        x2meno1=Line(self.coords_to_point(8,4*.5-5*.5+2),self.coords_to_point(8,.5*8-5*.5+2)).set_color(RED)
        self.play(yy2.move_to,self.coords_to_point(4,4*.5-5*.5+2))
        ty2meno1=TexMobject(r"x_2-x_1").next_to(y2meno1,DOWN)
        ty2meno1[0:2].set_color(PURPLE)
        ty2meno1[3:5].set_color(ORANGE)
        self.play(ShowCreation(y2meno1,run_time=2),TransformFromCopy(tx2[0],ty2meno1[0]),TransformFromCopy(tx2[1],ty2meno1[1]),ShowCreation(ty2meno1[2]),TransformFromCopy(tx1[0],ty2meno1[3]),TransformFromCopy(tx1[1],ty2meno1[4]))
        self.play(xx1.move_to,self.coords_to_point(8,(4*.5-5*.5+2)/2))
        tx2meno1=TexMobject(r"y_2-y_1").next_to(x2meno1,RIGHT)
        tx2meno1[0:2].set_color(PURPLE)
        tx2meno1[3:5].set_color(ORANGE)        
        self.play(ShowCreation(x2meno1,run_time=2),TransformFromCopy(ty2[0],tx2meno1[0]),TransformFromCopy(ty2[1],tx2meno1[1]),ShowCreation(tx2meno1[2]),TransformFromCopy(ty1[0],tx2meno1[3]),TransformFromCopy(ty1[1],tx2meno1[4]))
        self.wait(1)
        m=TexMobject(r"m=\frac{y_2-y_1}{x_2-x_1}").to_edge(RIGHT,buff=1).shift(1*DOWN)
        m[0].set_color(MAROON)
        m[2:4].set_color(PURPLE)
        m[5:7].set_color(ORANGE)
        m[8:10].set_color(PURPLE)
        m[11:13].set_color(ORANGE)
        self.play(Write(m[0]))
        self.play(ShowCreation(m[1]))
        self.play(tx2meno1.move_to,m[2:7],ShowCreation(m[7]),ty2meno1.move_to,m[8:13])
        r=SurroundingRectangle(m).set_color(PINK)
        ang=TextMobject(r"\textbf{Incremental Ratio}").set_color(PINK).scale(.6).next_to(r,UP)
        self.play(FadeInFromDown(ang))
        self.play(ShowCreationThenDestruction(r))
        self.play(ReplacementTransform(tx2meno1,m[2:7]),ReplacementTransform(ty2meno1,m[8:13]))
        fx=TexMobject(r"f(x)").set_color(GREEN).next_to(funz,UP)
        self.play(FadeOutAndShift(ang,UP))
        self.wait()
        self.play(ShowCreation(funz),Write(fx))
        self.wait(1)
        fx1=TexMobject(r"f(x_1)").next_to(y1,LEFT)
        fx1[0].set_color(GREEN)
        fx1[2:4].set_color(ORANGE)
        fx2=TexMobject(r"f(x_2)").next_to(y2,LEFT)
        fx2[0].set_color(GREEN)
        fx2[2:4].set_color(PURPLE)
        self.play(ReplacementTransform(ty1,fx1),ReplacementTransform(ty2,fx2))
        mm=TexMobject(r"\frac{f(x_2)-f(x_1)}{x_2-x_1}").move_to(m[2:13])
        mm[0].set_color(GREEN)
        mm[2:4].set_color(PURPLE)
        mm[6].set_color(GREEN)
        mm[8:10].set_color(ORANGE)
        self.play(ReplacementTransform(m[5:7],mm[6:11]),ReplacementTransform(m[2:4],mm[0:5]),ReplacementTransform(m[4],mm[5]),ReplacementTransform(m[7],mm[11]),m[0:2].next_to,mm[11],LEFT)
        def vert1(u=4):
         return self.get_vertical_line_to_graph(u,funz,line_class=DashedLine).set_color(YELLOW)
        X1=vert1()
        def update_vert1(X1,alpha):
         u=interpolate(4,6,alpha)
         XX1=vert1(u)
         X1.become(XX1)
        def vert2(u=8):
         return self.get_vertical_line_to_graph(u,funz,line_class=DashedLine).set_color(YELLOW)
        X2=vert2()
        def update_vert2(X2,alpha):
         u=interpolate(8,6,alpha)
         XX2=vert2(u)
         X2.become(XX2)
        def orizz1(x=4):
         return DashedLine(self.coords_to_point(0,-3/128*x**3+21/64*x**2-13/16*x+1),self.coords_to_point(x,-3/128*x**3+21/64*x**2-13/16*x+1)).set_color(YELLOW)
        Y1=orizz1()
        def update_orizz1(Y1,alpha):
         u=interpolate(4,6,alpha)
         YY1=orizz1(u)
         Y1.become(YY1)
        def orizz2(x=8):
         return DashedLine(self.coords_to_point(0,-3/128*x**3+21/64*x**2-13/16*x+1),self.coords_to_point(x,-3/128*x**3+21/64*x**2-13/16*x+1)).set_color(YELLOW)
        Y2=orizz2()
        def update_orizz2(Y2,alpha):
         u=interpolate(8,6,alpha)
         YY2=orizz2(u)
         Y2.become(YY2)
        def tangent(u=2):
            return self.get_graph(lambda x: (x-6+u)/(2*u)*(-3/128*(6+u)**3+21/64*(6+u)**2-13/16*(6+u)+1-(-3/128*(6-u)**3+21/64*(6-u)**2-13/16*(6-u)+1))+-3/128*(6-u)**3+21/64*(6-u)**2-13/16*(6-u)+1).set_color(RED)
        t=tangent()
        def update_tangent(t,alpha):
         u=interpolate(2,.1,alpha)
         tt=tangent(u)
         t.become(tt)
        self.remove(xx1,yy2)
        self.wait()
        ang2=TextMobject(r"\textbf{Secant Line}").set_color(RED).scale(.7).move_to(funz).rotate(np.arctan(.73),axis=Z_AXIS).shift(1.5*RIGHT+.3*UP)
        ang3=TextMobject(r"\textbf{Tangent Line?}").set_color(RED).scale(.7).move_to(ang2).rotate(np.arctan(.73),axis=Z_AXIS)
        self.play(Write(ang2))
        self.wait()
        self.play(Transform(ang2,ang3))
        self.play(FadeOut(ang2),ReplacementTransform(x1,X1),ReplacementTransform(x2,X2),ReplacementTransform(y1,Y1),ReplacementTransform(y2,Y2),ReplacementTransform(f,t),FadeOut(y2meno1),FadeOut(x2meno1))
        tx1.add_updater(lambda m: m.next_to(X1,DOWN))
        tx2.add_updater(lambda m: m.next_to(X2,DOWN))
        fx1.add_updater(lambda m: m.next_to(Y1,LEFT))
        fx2.add_updater(lambda m: m.next_to(Y2,LEFT))
        self.play(UpdateFromAlphaFunc(X1,update_vert1),UpdateFromAlphaFunc(X2,update_vert2),UpdateFromAlphaFunc(Y1,update_orizz1),UpdateFromAlphaFunc(Y2,update_orizz2),UpdateFromAlphaFunc(t,update_tangent),run_time=6)
        self.wait(1)
        self.play(FadeOut(scritta))
        der=TextMobject("DERIVATIVE").set_color_by_gradient(PURPLE,ORANGE).to_edge(UP).scale(1.9)
        self.play(FadeInFromDown(der))
        self.wait(1)
        gruppone=VGroup(self.x_axis,self.y_axis,self.axes,tx1,tx2,fx1,fx2,fx)
        M=VGroup(m[0:2],mm[0:12],m[7:13])
        self.play(FadeOut(gruppone),Uncreate(X1),Uncreate(X2),Uncreate(Y1),Uncreate(t),Uncreate(Y2),Uncreate(funz))
        funz1=self.get_graph(lambda x: -3/128*x**3+21/64*x**2-13/16*x+1,x_min=0,color=GREEN)
        ###
        def deriv1(k=.1):
            return self.get_graph(lambda x: (-9/128*k**2+42/64*k-13/16)*(x-k)-3/128*k**3+21/64*k**2-13/16*k+1,x_min=k-1.5,x_max=k+1.5,color=RED)
        l=deriv1()
        L=VGroup(*list(deriv1(.1+i/2) for i in range(1,12)))
        def vl1(k=.1):
            return self.get_vertical_line_to_graph(k,funz1,line_class=DashedLine).set_color(YELLOW)
        lallo=vl1()
        funz2=self.get_graph(lambda x: -9/128*x**2+42/64*x-13/16+2,x_min=0,color=RED)
        def vl2(k2=.1):
            return DashedLine(self.coords_to_point(k2,0),self.coords_to_point(k2,-9/128*k2**2+42/64*k2-13/16+2)).set_color(YELLOW)
        lallo2=vl2()
        def update_deriv1(l,alpha):
            kk=interpolate(.1,8,alpha)
            ll=deriv1(kk)
            l.become(ll)
        def update_vl1(lallo,alpha):
            kk1=interpolate(.1,8,alpha)
            vl11=vl1(kk1)
            lallo.become(vl11)
        def update_vl2(lallo2,alpha):
            kk2=interpolate(.1,8,alpha)
            vl22=vl2(kk2)
            lallo2.become(vl22)
        ###
        lal1=vl1(k=4.1)
        lal2=vl2(k2=4.1)
        x2x0=TexMobject(r"x_1").next_to(lal2,DOWN).set_color(ORANGE)
        x1x0=TexMobject(r"x_1").next_to(lal1,DOWN).set_color(ORANGE)
        ol=DashedLine(self.coords_to_point(4.1,-9/128*4.1**2+42/64*4.1-13/16+2),self.coords_to_point(0,-9/128*4.1**2+42/64*4.1-13/16+2)).set_color(YELLOW)
        fol=TexMobject(r"f'(x_1)").next_to(ol,LEFT)
        fol[3:5].set_color(ORANGE)
        L8=self.get_graph(lambda x: (-9/128*4.1**2+42/64*4.1-13/16)*(x-4.1)-3/128*4.1**3+21/64*4.1**2-13/16*4.1+1,x_min=2.1,x_max=6.1,color=YELLOW)
        x1x1=TexMobject(r"x").next_to(lallo,DOWN).add_updater(lambda m: m.next_to(lallo,DOWN))
        x2x2=TexMobject(r"x").next_to(lallo,DOWN).add_updater(lambda m: m.next_to(lallo2,DOWN))
        dotti=VGroup(*list(Dot(self.coords_to_point(.1+i/2,-9/128*(.1+i/2)**2+42/64*(.1+i/2)-13/16+2)).scale(.6).set_color(RED) for i in range(0,11)))
        self.play(M.move_to,(-3,1.5,0))
        self.setup_axes(animate=False)
        gr2=VGroup(self.x_axis,self.y_axis,self.axes)
        G2=VGroup(gr2,funz2,dotti,lallo2,x2x2,x2x0,ol,fol,lal2).scale(.65).to_corner(DR) 
        self.setup_axes(animate=False)
        gr1=VGroup(self.x_axis,self.y_axis,self.axes)
        G1=VGroup(gr1,funz1,l,L,lallo,x1x1,L8,x1x0,L8,lal1).scale(.65).to_corner(DL)
        deriv=TexMobject(r"=\frac{\Delta f}{\Delta x}").next_to(M,buff=SMALL_BUFF)
        self.play(ShowCreation(deriv[0]))
        self.play(Write(deriv[1:6]))
        d=TexMobject(r"=\frac{d f}{d x}").move_to(deriv)
        self.wait(1)
        self.play(ReplacementTransform(deriv,d))
        self.wait(1)
        self.play(FadeInFromDown(gr1),FadeInFromDown(gr2))
        self.play(ShowCreation(funz1))
        self.wait()
        for i in range(0,11):
            self.play(ShowCreation(L[i]),FadeInFromDown(dotti[i]),run_time=.5)
        self.play(FadeOut(L),FadeOut(dotti))
        self.play(FadeInFromDown(x1x1),ShowCreation(lallo),ShowCreation(l))
        self.play(UpdateFromAlphaFunc(lallo,update_vl1),UpdateFromAlphaFunc(l,update_deriv1),ShowCreation(funz2),rate_func=linear,run_time=4)
        self.play(FadeOut(x1x1),FadeOut(lallo),FadeOut(l))
        self.wait()
        ugfp=TexMobject(r"=f'(x)").next_to(d,buff=SMALL_BUFF)
        self.play(ShowCreation(ugfp[0]))
        self.play(ShowCreation(ugfp[1:6]))
        self.wait()
        gg=ugfp[1:6].copy()
        self.play(gg.next_to,funz2,UP,buff=SMALL_BUFF)
        R=SurroundingRectangle(gg).set_color(PINK)
        self.play(ShowCreationThenDestruction(R))
        self.wait()
        self.play(ShowCreation(L[7]),ShowCreation(lal1),ShowCreation(lal2),FadeIn(x1x0),FadeIn(x2x0))
        self.wait()
        self.play(Transform(L[7],L8),rate_func=there_and_back)
        self.wait()
        self.play(ShowCreation(ol))
        self.play(TransformFromCopy(gg,fol))
        self.wait()
        
class WhyCare(Scene):
    def construct(self):
        t1=TexMobject(r"\frac{dx^n}{dx}=nx^{n-1}").scale(2)
        t2=TexMobject(r"\frac{d\ln(x)}{dx}=\frac{1}{x}").scale(2)
        t3=TexMobject(r"\frac{d\sin(x)}{dx}=\cos(x)").scale(2)
        t4=TexMobject(r"\frac{d\cos(x)}{dx}=-\sin(x)").scale(2)
        t5=TexMobject(r"\frac{de^x}{dx}=e^x").scale(2)
        s=TextMobject(r"Why do we care \\ about derivatives?").scale(3).set_color(YELLOW)
        self.play(FadeInFromDown(t1))
        self.wait()
        self.play(ReplacementTransform(t1,t2))
        self.wait()
        self.play(ReplacementTransform(t2,t3))
        self.wait()
        self.play(ReplacementTransform(t3,t4))
        self.wait()
        self.play(ReplacementTransform(t4,t5))
        self.wait()
        self.play(FadeOutAndShiftDown(t5))
        self.play(FadeInFrom(s[0:3],LEFT),run_time=.3)
        self.play(FadeInFromLarge(s[3:5],.1),run_time=.3)
        self.play(FadeInFromLarge(s[5:7],.1),run_time=.3)
        self.play(FadeInFromLarge(s[7:11],.1),run_time=.3)
        self.play(FadeInFrom(s[11:16],LEFT),run_time=.3)
        self.play(FadeInFrom(s[16:28],RIGHT),run_time=.3)
        self.wait()
        
class Extend(Scene):
    def construct(self):
        f11=TexMobject(r"f(x)").scale(1.3)
        f12=TexMobject(r"f'(x_0)=0").scale(1.3).next_to(f11,DOWN,buff=1)
        f12[3:5].set_color(GREEN)
        f13=TexMobject(r"f''(x_0)\gtrless 0").scale(1.3).next_to(f12,DOWN,buff=1)
        f13[4:6].set_color(GREEN)
        f=VGroup(f11,f12,f13).move_to((-4,0,0))
        f21=TexMobject(r"f(x,y)").scale(1.3)
        f22=TexMobject(r"\frac{\partial f}{\partial x} \,\,\, \frac{\partial f}{\partial y}").scale(1.3).next_to(f21,DOWN,buff=1)
        f23=TexMobject(r"\frac{\partial^2 f}{\partial x^2} \,\,\, \frac{\partial^2 f}{\partial x\partial y} \,\,\, \frac{\partial^2 f}{\partial y^2}").scale(1.3).next_to(f22,DOWN,buff=1)
        f2=VGroup(f21,f22,f23).move_to((4,0,0))
        r=SurroundingRectangle(f).set_color(PINK)
        r2=SurroundingRectangle(f2).set_color(PINK)
        meh=TextMobject("Meh...").next_to(r,UP)
        yeh=TextMobject("Yeh...").next_to(r2,UP)
        self.play(FadeIn(f),run_time=2)
        self.play(ShowCreation(r))
        self.play(FadeInFrom(meh,UP))
        self.wait(2)
        self.play(FadeInFromDown(f2))
        self.play(ShowCreation(r2))
        self.play(FadeInFrom(yeh,UP))
        self.wait()
        a=Arrow(r,r2).set_color(YELLOW)
        self.play(GrowArrow(a))
        self.wait(2.11)

class How(Scene):
   def construct(self): 
     hw=TextMobject("How can we do this?").scale(3).to_edge(UP,buff=1).set_color(YELLOW)
     hw2=TextMobject(r"How is this matrix connected \\ to the study of maxima \\ and minima?").scale(2).next_to(hw,DOWN,buff=1).set_color(YELLOW)
     self.play(FadeInFrom(hw[0:3],LEFT),run_time=.25)
     self.play(FadeInFromLarge(hw[3:6],.1),run_time=.25)
     self.play(FadeInFromLarge(hw[6:8],.1),run_time=.25)
     self.play(FadeInFromLarge(hw[8:10],.1),run_time=.25)
     self.play(FadeInFrom(hw[10:15],RIGHT),run_time=.25)
     self.wait(.3)
     self.play(FadeInFrom(hw2[0:3],LEFT),run_time=.25)
     self.play(FadeInFromLarge(hw2[3:5],.1),run_time=.25)
     self.play(FadeInFromLarge(hw2[5:9],.1),run_time=.25)
     self.play(FadeInFromLarge(hw2[9:15],.1),run_time=.25)
     self.play(FadeInFrom(hw2[15:24],RIGHT),run_time=.5)
     self.play(FadeInFrom(hw2[24:26],LEFT),run_time=.5)
     self.play(FadeInFromLarge(hw2[26:29],.1),run_time=.25)
     self.play(FadeInFromLarge(hw2[29:34],.1),run_time=.25)
     self.play(FadeInFromLarge(hw2[34:36],.1),run_time=.25)
     self.play(FadeInFromLarge(hw2[36:42],.1),run_time=.25) 
     self.play(FadeInFrom(hw2[42:45],LEFT),run_time=.25)
     self.play(FadeInFrom(hw2[45:52],RIGHT),run_time=.25)
     self.wait()
class hi(Scene):
    def construct(self):
     t=TextMobject(r"Hi!").scale(7)
     self.play(FadeInFromDown(t),run_time=.5)
     self.wait(2)

class eig0(Scene):
    def construct(self):
        H=TexMobject(r"H=\left(\begin{matrix}f_{xx}&&f_{xy} \\\\ f_{yx}&&f_{yy}\end{matrix}\right)").scale(2)
        H[0:2].set_color(BLACK)
        ale=Alex().scale(.7).to_corner(DL)
        ale[4].set_color(BLUE)
        self.add(ale,H)
        H[4:7].set_color(RED)
        H[7:10].set_color(YELLOW)
        H[10:13].set_color(YELLOW)
        H[13:16].set_color(BLUE)
        tt=TexMobject(r"\text{If\,\,} \lambda_{1,2}=0 \text{,\,the Hessian Method is Inconclusive}").scale(1.3).set_color(PINK).to_edge(UP)
        self.play(Write(tt),run_time=3)
        palabras=TextMobject("What is its sign?")
        self.play(NumberCreatureSays(ale,palabras, bubble_kwargs = {"height" : 2, "width" : 3}))
        self.wait()
        self.play(Blink(ale))
        self.wait()
        w=TextMobject("Who Cares?").move_to(palabras)
        fbar=TexMobject(r"\bar{f}(x,y)=f(x,y)-f(x_0,y_0)\geq 0").scale(1.6).shift(1.3*UP)
        fbar[0:2].set_color(RED)
        fbar[17:19].set_color(GREEN)
        fbar[20:22].set_color(PURPLE)
        self.play(Transform(palabras,w),Transform(H[2:18],fbar[0:23]))
        self.wait()
        self.play(ShowCreation(fbar[23]))
        self.play(Write(fbar[24]),Blink(ale))
        self.wait()
        self.play(Blink(ale))
        self.wait()

class Hessian(Scene):#fare scena in cui ripassare i paraboloidi? beh si, anzi fai notare che i paraboloidi con segno discorde hanno le selle
    def construct(self):
        self.wait()
        a=TexMobject(r"f(x)\approx f(x_0)+\frac{1}{2}f''(x_0)(x-x_0)^2").to_corner(UL) #parabol
        a[7:9].set_color(GREEN)
        a[14:17].set_color(RED)
        a[18:20].set_color(GREEN)
        a[24:26].set_color(GREEN)
        ta=TextMobject("parabola").next_to(a,buff=3).set_color_by_gradient(BLUE,GREEN)
        A=TexMobject(r"f(x,y)\approx f(x_0,y_0)+\frac{1}{2}\Big(f_{xx}(x_0,y_0)(x-x_0)^2+ \\ 2f_{xy}(x_0,y_0)(x-x_0)(y-y_0)+f_{yy}(x_0,y_0)(y-y_0)^2\Big)").to_edge(LEFT,buff=1).shift(1*UP)
        A[0:38].shift(2*LEFT)
        A[9:11].set_color(GREEN)
        A[12:14].set_color(PURPLE)
        A[20:23].set_color(RED)
        A[24:26].set_color(GREEN)
        A[27:29].set_color(PURPLE)
        fxx=A[20:30].copy()
        A[33:35].set_color(GREEN)
        A[38:42].set_color(YELLOW)
        A[43:45].set_color(GREEN)
        A[46:48].set_color(PURPLE)
        fxy=A[39:49].copy()
        fyx=A[39:49].copy()
        A[52:54].set_color(GREEN)
        A[58:60].set_color(PURPLE)
        A[62:65].set_color(BLUE)
        A[66:68].set_color(GREEN)
        A[69:71].set_color(PURPLE)
        fyy=A[62:72].copy()
        A[75:77].set_color(PURPLE)
        tp=TexMobject(r"z=z_0+a(x-x_0)^2+b(y-y_0)^2 \\ +2c(x-x_0)(y-y_0)").scale(1.3).to_edge(LEFT).shift(1.4*DOWN)
        tp[5].set_color(RED)
        tp[9:11].set_color(GREEN)
        tp[14].set_color(BLUE)
        tp[18:20].set_color(PURPLE)
        tp[23:25].set_color(YELLOW)
        tp[28:30].set_color(GREEN)
        tp[34:36].set_color(PURPLE)
        tA=TextMobject("paraboloid").next_to(ta,DOWN,buff=1).shift(.1*RIGHT).set_color_by_gradient(BLUE,GREEN)
        self.play(Write(a))
        self.play(Write(ta))
        self.play(Write(A))
        self.play(Write(tA))
        self.wait(1)
        self.play(FadeInFromDown(tp))
        self.wait(1)
        self.play(tp[5].scale,2,tp[14].scale,2,tp[23:25].scale,2,A[20:23].scale,2,A[62:65].scale,2,A[38:42].scale,2,run_time=2.5,rate_func=there_and_back)
        self.wait()
        H=TexMobject(r"H_{(x_0,y_0)}=\left(\begin{matrix}f_{xx}(x_0,y_0)&f_{xy}(x_0,y_0)\\\\\\f_{xy}(x_0,y_0)&f_{yy}(x_0,y_0)\end{matrix}\right)").to_corner(DL,buff=1).shift(.4*DOWN)
        H[2:4].set_color(GREEN)
        H[5:7].set_color(PURPLE)
        self.play(FadeOutAndShiftDown(tp),fxx.move_to,H[13:23],fxy.move_to,H[23:33],fyx.move_to,H[33:43],fyy.move_to,H[43:53],run_time=1.7)
        self.play(FadeIn(H[9:13]),FadeIn(H[53:57]))
        self.wait(1)
        brace=Brace(mobject=H[9:57],direction=UP).set_color(PINK)
        t=brace.get_text(r"Hessian Matrix").set_color(PINK).shift(.1*DOWN)
        self.play(GrowFromCenter(brace))
        self.play(Write(t))
        self.play(ShowCreation(H[8]))
        self.play(Write(H[0:8]))
        self.play(FadeOut(brace),FadeOut(t))
        self.wait(1)
        ale=Alex().rotate(PI,axis=Y_AXIS).move_to((10,-2,0)).scale(.7)
        ale[4].set_color(BLUE)
        ale[2:4].shift(.025*DOWN+0.015*LEFT)
        self.play(ale.move_to,(4,-2,0))
        p=TextMobject(r"?").next_to(ale,UR,buff=0).scale(1.4)
        self.play(FadeIn(p))
        self.wait(1)
        self.play(Blink(ale))
        v=VGroup(fxx,fxy,fyx,fyy)
        V=VGroup(v,H[0:13],H[53:57])
        self.play(FadeOut(ta),FadeOut(tA),FadeOut(a),FadeOut(A),V.shift,3*UP,ale[2:4].shift,.05*UP+0.015*RIGHT)
        dest=VGroup(fxy[0:3],fyy[0:3])
        cart=TextMobject("Descartes' Rule").scale(3).to_edge(UP).set_color_by_gradient(BLUE,GREEN)
        self.play(FadeInFromDown(cart))
        self.wait(1)
        self.play(Blink(ale),FadeOut(fxx[3:10]),FadeOut(fxy[3:10]),FadeOut(fyx[3:10]),FadeOut(fyy[3:10]),dest.shift,LEFT,H[53:57].shift,2.5*LEFT)
        #togliamo la scritta x0y0 per avvrebiare
        l=TexMobject(r"-\,\lambda").next_to(fxx[0:3],buff=SMALL_BUFF)
        ll=l.copy().next_to(fyy[0:3],buff=SMALL_BUFF)
        det=TexMobject(r"\det").move_to(H[8])
        self.play(fyx[0:3].shift,.4*RIGHT,fxy[0:3].shift,.4*RIGHT,Transform(H[0:9],det),FadeInAndShiftFromDirection(l,RIGHT),FadeInAndShiftFromDirection(ll,RIGHT),H[53:57].shift,.8*RIGHT)
        ugu0=TexMobject(r"=0").next_to(H[53:57])
        self.play(ShowCreation(ugu0[0]))
        self.play(Write(ugu0[1]),FadeOut(p))
        self.wait(1)
        ls=TextMobject("Eigenvalues").next_to(ugu0[1],buff=2).set_color_by_gradient(MAROON,BLUE)
        ar=Arrow(ugu0[1],ls[0]).set_color(YELLOW)
        self.play(GrowArrow(ar))
        self.play(Blink(ale),FadeInAndShiftFromDirection(ls,RIGHT))
        self.wait(1)
        self.play(Blink(ale))
        self.wait(1)
#RICORDA SCENA MASSIMO SU UN INTERA LINEA
    
#fai una scena in cui usi value tracker e fai vedere come cambia il paraboloide se cambiamo i segni, in alcuni casi diventa minimo, in atri sella e in altri massimo
#c'è un modo di poter formalizzare tutto questo con le derivate parziali? sì tramite l'hessiano! e parti con la scena hessian, dove dici che per motivi rigurdanti taylor, 
#una funzione può essere approssimata nell'intorno di un suo estremo, a questo se è una variabile o a quest'altro se è a due variabili, ma questo è
#riscrivibile come un prodotto tra matrice e bla bla (parla dell'hessiano)
#in che modo ci aiuta? beh grazie a lui siamo in grado di capire quale termine è dominante, e quindi il comportamenteo del paraboloide osculatore
#per farlo ci avvaliamo del criterio di Sylvester che ci fa capire come per una matrice SIMMETRICA (cita SHWARZ) gli autovalori determinano il segno della matrice
#è un modo elegante per ritornare al caso a una variabile non solo formalmente, ma proprio di operazioni.

class Paraboloid(ThreeDScene):
    CONFIG = {
        "rows":20,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
     }
    def construct(self):
        self.move_camera(phi=70*DEGREES, theta=21*DEGREES)
        self.begin_ambient_camera_rotation(rate=0.0175)
        axes=ThreeDAxes()
        def paraboloid(a=-1):
            return ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                a*(u-2)**2+a*(v-2)**2+0*(u-2)*(v-2)+1
             ]),v_min=2-1.3,v_max=2+1.3,u_min=2-1.3,u_max=2+1.3,checkboard_colors=[BLUE_D,BLUE_E],
             resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
        f=paraboloid()
        tp=TexMobject(r"z=a(x-x_0)^2+b(y-y_0)^2 \\ +2c(x-x_0)(y-y_0)+z_0") #computa con tex=0
        r2=SurroundingRectangle(tp).set_color(PINK)
        tp[2].set_color(RED)
        tp[6:8].set_color(GREEN)
        tp[11].set_color(BLUE)
        tp[15:17].set_color(PURPLE)
        tp[20:22].set_color(YELLOW)
        tp[25:27].set_color(GREEN)
        tp[31:33].set_color(PURPLE)
        a = DecimalNumber(-1)
        time1=ValueTracker(-1)
        ta=TexMobject(r"a=")
        def numUpdatera(num):
            num.set_value(time1.get_value())
            self.add_fixed_in_frame_mobjects(num)

        a.add_updater(numUpdatera).set_color(RED).to_edge(LEFT).shift(1.5*RIGHT+UP)
        b = DecimalNumber(-1)
        time2=ValueTracker(-1)
        tb=TexMobject(r"b=")
        def numUpdaterb(num):
            num.set_value(time2.get_value())
            self.add_fixed_in_frame_mobjects(num)

        b.add_updater(numUpdaterb).set_color(BLUE).to_edge(LEFT).shift(1.5*RIGHT)
        c = DecimalNumber(0)
        time3=ValueTracker(0)
        tc=TexMobject(r"c=")
        def numUpdaterc(num):
            num.set_value(time3.get_value())
            self.add_fixed_in_frame_mobjects(num)

        c.add_updater(numUpdaterc).set_color(YELLOW).to_edge(LEFT).shift(1.5*RIGHT+DOWN)
        self.add_fixed_in_frame_mobjects(tp,ta,tb,tc,r2)
        ta.next_to(a,LEFT,buff=SMALL_BUFF).set_color(RED)
        tb.next_to(b,LEFT,buff=SMALL_BUFF).set_color(BLUE)
        tc.next_to(c,LEFT,buff=SMALL_BUFF).set_color(YELLOW)
        tp.to_corner(UL)
        r2.move_to(tp)
        grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        grilla2=grilla.copy().next_to(grilla,UP,buff=0)
        grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
        grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
        grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
        grilla6=grilla.copy().next_to(grilla,RIGHT+DOWN,buff=0)
        grilla7=NumberPlane(width=24,height=4,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke).next_to(grilla,RIGHT,buff=0)
        grilla8=grilla.copy().next_to(grilla,LEFT,buff=0)
        x0=TexMobject(r"x_0").rotate(PI/2,axis=Z_AXIS).set_color(GREEN)
        lx0=DashedLine((2,0,0),(2,2,0)).set_color(YELLOW)
        x0.next_to(lx0,DOWN)
        y0=TexMobject(r"y_0").rotate(PI/2,axis=Z_AXIS).set_color(PURPLE)
        ly0=DashedLine((0,2,0),(2,2,0)).set_color(YELLOW)
        y0.next_to(ly0,LEFT)
        z0=TexMobject(r"z_0").rotate(PI/2,axis=Z_AXIS).rotate(PI/2,axis=Y_AXIS)
        lz0=DashedLine((2,2,0),(2,2,1)).set_color(YELLOW)
        z0.next_to(lz0,DOWN)
        self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8,f,a,ta,b,tb,c,tc,tp)        
        def update_paraboloid(f,alpha):
            aa=interpolate(-1,1,alpha)
            ff=paraboloid(aa)
            f.become(ff)
        self.wait(2)
        self.play(ShowCreationThenDestruction(r2))
        self.play(tp[2].scale,2.8,tp[11].scale,2.8,tp[21].scale,2.8,run_time=1.7,rate_func=there_and_back)
        self.play(FadeIn(x0),FadeIn(y0),ShowCreation(lx0),ShowCreation(ly0))
        self.play(FadeIn(z0),ShowCreation(lz0))
        scritta1=TexMobject(r"(x_0,y_0)\text{\,is a Maximum Point\,MinimumSaddle}").scale(1.4).set_color(PINK)
        scritta1[22:29].move_to(scritta1[10:17])
        scritta1[0].set_color(WHITE)
        scritta1[1:3].set_color(GREEN)
        scritta1[3].set_color(WHITE)
        scritta1[4:6].set_color(PURPLE)
        scritta1[6].set_color(WHITE)
        S=VGroup(scritta1)
        self.add_fixed_in_frame_mobjects(S)
        scritta1.to_corner(DL)
        scritta1[29:35].next_to(scritta1[0:10]).shift(.068*UP)
        self.play(Write(scritta1[0:22]))
        self.wait(1)
        self.play(UpdateFromAlphaFunc(f,update_paraboloid),time1.set_value,1,time2.set_value,1,run_time=5)
        self.play(Transform(scritta1[10:17],scritta1[22:29]))
        def paraboloid2(b=1):
            return ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                (u-2)**2+b*(v-2)**2+0*(u-2)*(v-2)+1
             ]),v_min=2-1.3,v_max=2+1.3,u_min=2-1.3,u_max=2+1.3,checkboard_colors=[BLUE_D,BLUE_E],
             resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
        f2=paraboloid2()
        def update_paraboloid2(f2,alpha):
            bb=interpolate(1,-.5,alpha)
            ff2=paraboloid2(bb)
            f2.become(ff2)
        self.play(ReplacementTransform(f,f2),run_time=.1)
        self.play(UpdateFromAlphaFunc(f2,update_paraboloid2),time2.set_value,-.5,run_time=5)
        l1=ParametricFunction(lambda u: np.array([u,2,(u-2)**2+1]),t_min=2-1.3,t_max=2+1.3).set_color(RED).set_stroke(width=2)
        l2=ParametricFunction(lambda v: np.array([2,v,-.5*(v-2)**2+1]),t_min=2-1.3,t_max=2+1.3).set_color(RED).set_stroke(width=2)
        self.play(ShowCreation(l2))
        self.wait(2)
        self.play(ShowCreation(l1))
        self.wait(2)
        self.play(FadeOut(scritta1[10:17]),FadeInFromDown(scritta1[29:35]),scritta1[17:22].shift,1.1*LEFT)
        self.wait(1)
        #so, how can we predict the andamento of the paraboloid? is there a pattern^--->hessian
        self.play(FadeOut(scritta1[0:10]),FadeOut(scritta1[17:22]),FadeOut(l1),FadeOut(l2),FadeOut(scritta1[29:35]))
        def paraboloid3(c=1):
            return ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                (u-2)**2-0.5*(v-2)**2+c*(u-2)*(v-2)+1
             ]),v_min=2-1.3,v_max=2+1.3,u_min=2-1.3,u_max=2+1.3,checkboard_colors=[BLUE_D,BLUE_E],
             resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
        f3=paraboloid3()
       # def update_paraboloid3(f3,alpha):
       #     cc=interpolate(0,1,alpha)
       #     ff3=paraboloid2(cc)
       #     f3.become(ff3)
        self.play(ReplacementTransform(f2,f3),time3.set_value,1,run_time=2.5)
        #def update_paraboloid4(f4,alpha):
        #    cc=interpolate(1,-.5,alpha)
        #    ff4=paraboloid2(cc)
        #    f4.become(ff4)
        f4=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                -.5*(u-2)**2+.5*(v-2)**2+1
             ]),v_min=2-1.3,v_max=2+1.3,u_min=2-1.3,u_max=2+1.3,checkboard_colors=[BLUE_D,BLUE_E],
             resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
        self.play(ReplacementTransform(f3,f4),time1.set_value,-.5,time3.set_value,0,run_time=2.5)
        f5=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                .5*(u-2)**2+.5*(v-2)**2+.25*(u-2)*(v-2)+1
             ]),v_min=2-1.3,v_max=2+1.3,u_min=2-1.3,u_max=2+1.3,checkboard_colors=[BLUE_D,BLUE_E],
             resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
        self.play(ReplacementTransform(f4,f5),time1.set_value,.5,time2.set_value,.5,time3.set_value,.25,run_time=2.5)
        self.wait(1)
        h=TexMobject(r"\left(\begin{matrix}a && c\\\\c && d\end{matrix}\right)").scale(1.6)
        sign=TextMobject(r"Sign of the matrix").scale(.8).set_color(PINK)
        r=SurroundingRectangle(h).set_color(PINK)
        self.add_fixed_in_frame_mobjects(h,r,sign)
        h.to_corner(DR)
        r.move_to(h)
        sign.next_to(h,UP,buff=SMALL_BUFF)
        h[2].set_color(RED)
        h[3].set_color(YELLOW)
        h[4].set_color(YELLOW)
        h[5].set_color(BLUE)
        tp2=tp[2].copy()
        tp11=tp[11].copy()
        tp20a=tp[21].copy()
        tp20b=tp[21].copy()
        V=VGroup(tp2,tp11,tp20a,tp20b)
        self.add_fixed_in_frame_mobjects(V)
        self.play(tp2.move_to,h[2],tp11.move_to,h[5],tp20a.move_to,h[3],tp20b.move_to,h[4],run_time=2.5)
        self.play(FadeIn(h[0:2]),FadeIn(h[6:8]),tp2.scale,1.6,tp11.scale,1.6,tp20a.scale,1.6,tp20b.scale,1.6)
        self.wait()
        self.play(ShowCreationThenDestruction(r))
        self.wait(2)
        self.play(FadeInFrom(sign,UP))
        self.wait(2)
        self.play(FadeOutAndShift(sign,UP))
        self.wait(2)

class congrats(Scene):
    def construct(self):
        ale=Alex().to_corner(DL,buff=0).scale(.6)
        ale[4].set_color(BLUE)
        im=ImageMobject("winners").scale(1.8).to_corner(UR,buff=2)
        r=SurroundingRectangle(im).set_color(WHITE).set_stroke(width=.5)
        self.add(ale,im,r)
        self.wait()
        p=TextMobject(r"Congrats to \\ the Winners!")
        self.play(NumberCreatureSays(ale,p))
        self.play(Blink(ale))
        self.wait()

class weap(Scene):
    def construct(self):
        ale=Alex().to_corner(DL,buff=0).scale(.6)
        ale[4].set_color(BLUE)
        im=ImageMobject("PartialD").scale(1.8).to_corner(UR).shift(.25*UP)
        r=SurroundingRectangle(im).set_color(WHITE).set_stroke(width=.5)
        v1=VGroup(im,r)
        im2=ImageMobject("SignHessian").scale(1.8).to_corner(UL).shift(.25*UP)
        r2=SurroundingRectangle(im2).set_color(WHITE).set_stroke(width=.5)
        v2=VGroup(im2,r2)
        im3=ImageMobject("HNullo2").scale(1.8).to_edge(DOWN,buff=SMALL_BUFF).shift(.1*UP)
        r3=SurroundingRectangle(im3).set_color(WHITE).set_stroke(width=.5)
        v3=VGroup(im3,r3)
        self.add(v1,v2,v3)
        self.play(FadeInFromLarge(ale,.1))
        self.wait()
        self.play(Blink(ale))
        self.wait()
        
class problem(Scene):
    def construct(self):
        ale=Alex().to_corner(DL,buff=0).scale(.6)
        ale[4].set_color(BLUE)
        im=ImageMobject("PartialD").scale(1.8).to_corner(UR,buff=2)
        r=SurroundingRectangle(im).set_color(WHITE).set_stroke(width=.5)
        self.add(ale,im,r)
        self.wait()
        p=TextMobject(r"Nice!")
        self.play(NumberCreatureSays(ale,p))
        self.play(Blink(ale))
        self.wait()
        pp=TextMobject(r"?").move_to(p).scale(2.3)
        self.play(Transform(p,pp))
        self.play(Blink(ale))
        self.wait()

class SignHessian(ThreeDScene):
    CONFIG = {
        "rows":8,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
    }
    def construct(self):
     self.move_camera(phi=70*DEGREES, theta=21*DEGREES)
     self.begin_ambient_camera_rotation(rate=0.01)
     axes = ThreeDAxes()
     grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
     grilla2=grilla.copy().next_to(grilla,UP,buff=0)
     grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
     grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
     grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
     grilla6=grilla.copy().next_to(grilla,RIGHT+DOWN,buff=0)
     grilla7=NumberPlane(width=24,height=4,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke).next_to(grilla,RIGHT,buff=0)
     grilla8=grilla.copy().next_to(grilla,LEFT,buff=0)
     H=TexMobject(r"H=\left(\begin{matrix}f_{xx}&&f_{xy} \\\\ f_{yx}&&f_{yy}\end{matrix}\right)\Rightarrow \text{$\lambda_{1,2}<0$}\Rightarrow \text{Maximum}\text{Saddle}\text{Minimum}\text{$\lambda_1<0\&\lambda_2>0$}\text{$\lambda_{1,2}>0$} ").scale(1.3)
     H[4:7].set_color(RED)
     H[7:10].set_color(YELLOW)
     H[10:13].set_color(YELLOW)
     H[13:16].set_color(BLUE)
     H[18:62].set_color(PURPLE)
     H[18].set_color(WHITE)
     H[25].set_color(WHITE)
     H[33:39].move_to(H[26:32])
     H[39:46].move_to(H[26:32])
     H[50].next_to(H[46:50],DOWN,buff=0)
     H[51:55].next_to(H[50],DOWN,buff=0)
     H[46:55].move_to(H[19:25])
     H[55:62].move_to(H[19:25])
     self.add_fixed_in_frame_mobjects(H)
     H.to_corner(DL,buff=SMALL_BUFF)
     self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8,H[0:33])
     surface=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(u)*np.sin(v)+2
            ]),v_min=-PI,v_max=PI,u_min=-PI,u_max=PI,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.9).set_color_by_gradient(GREEN,BLUE)
     p1=Sphere(radius=.03).set_color(RED).move_to((0,PI/2,3))
     p2=p1.copy().move_to((PI/2,0,2))
     p3=p1.copy().move_to((0,-PI/2,1))
     self.add(surface,p1,p2,p3)
     def osculating(t=PI/2):
         return ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(PI/2*np.cos(t))*np.sin(PI/2*np.sin(t))+2-np.sin(PI/2*np.cos(t))*np.sin(PI/2*np.sin(t))*(u-PI/2*np.cos(t))+np.cos(PI/2*np.cos(t))*np.cos(PI/2*np.sin(t))*(v-PI/2*np.sin(t))
                +.5*(-np.cos(PI/2*np.cos(t))*np.sin(PI/2*np.sin(t))*(u-PI/2*np.cos(t))**2-np.cos(PI/2*np.cos(t))*np.sin(PI/2*np.sin(t))*(v-PI/2*np.sin(t))**2-np.sin(PI/2*np.cos(t))*np.cos(PI/2*np.sin(t))*(u-PI/2*np.cos(t))*(v-PI/2*np.sin(t)))
            ]),v_min=PI/2*np.sin(t)-1.3,v_max=PI/2*np.sin(t)+1.3,u_min=PI/2*np.cos(t)-1.3,u_max=PI/2*np.cos(t)+1.3,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.6).set_color(PURPLE)
     P=osculating()
     self.add(P)
     def update_osc(P,alpha):
            tt=interpolate(PI/2,0,alpha)
            PP=osculating(tt)
            P.become(PP)
     def linex(t=PI/2):
         return DashedLine((PI/2*np.cos(t),0,0),(PI/2*np.cos(t),PI/2*np.sin(t),0)).set_color(YELLOW)
     lx=linex()
     def liney(t=PI/2):
         return DashedLine((0,PI/2*np.sin(t),0),(PI/2*np.cos(t),PI/2*np.sin(t),0)).set_color(YELLOW)
     ly=liney()
     def linez(t=PI/2):
         return DashedLine((PI/2*np.cos(t),PI/2*np.sin(t),0),(PI/2*np.cos(t),PI/2*np.sin(t),np.cos(PI/2*np.cos(t))*np.sin(PI/2*np.sin(t))+1.9)).set_color(PURPLE)
     lz=linez()
     def updating_linex(lx,alpha):
         tx=interpolate(PI/2,0,alpha)
         llx=linex(tx)
         lx.become(llx)
     def updating_liney(ly,alpha):
         ty=interpolate(PI/2,0,alpha)
         lly=liney(ty)
         ly.become(lly)
     def updating_linez(lz,alpha):
         tz=interpolate(PI/2,0,alpha)
         llz=linez(tz)
         lz.become(llz)     
     self.play(ShowCreation(lx),ShowCreation(ly),ShowCreation(lz))
     self.wait(3)
     self.play(UpdateFromAlphaFunc(lx,updating_linex),UpdateFromAlphaFunc(ly,updating_liney),UpdateFromAlphaFunc(lz,updating_linez),UpdateFromAlphaFunc(P,update_osc),run_time=3,rate_func=linear)
     self.play(FadeInFromDown(H[33:39]),FadeInFromDown(H[46:55]),FadeOutAndShiftDown(H[19:25]),FadeOutAndShiftDown(H[26:33]))
     def update_osc2(P,alpha):
            tt=interpolate(0,-PI/2,alpha)
            PP=osculating(tt)
            P.become(PP)
     def updating_linex2(lx,alpha):
         tx=interpolate(0,-PI/2,alpha)
         llx=linex(tx)
         lx.become(llx)
     def updating_liney2(ly,alpha):
         ty=interpolate(0,-PI/2,alpha)
         lly=liney(ty)
         ly.become(lly)
     def updating_linez2(lz,alpha):
         tz=interpolate(0,-PI/2,alpha)
         llz=linez(tz)
         lz.become(llz)  
     self.play(UpdateFromAlphaFunc(lx,updating_linex2),UpdateFromAlphaFunc(ly,updating_liney2),UpdateFromAlphaFunc(lz,updating_linez2),UpdateFromAlphaFunc(P,update_osc2),run_time=3,rate_func=linear)
     self.play(FadeOutAndShiftDown(H[33:39]),FadeOutAndShiftDown(H[46:55]),FadeInFromDown(H[39:46]),FadeInFromDown(H[55:62]))
     self.wait(8)

#1)Finisci scena derivata seconda parabola FATTO
#2)Finisci scena Hessiano (regola cartesio) FATTO
#3)fai scena esercizio (dove metti regola) (approfittante aggiungendo caso in cui tutte le x sono massimi) FATTO
#4)suddividi tra massimi assoluti e relativi  FATTO
#5)doppia
#6)arricchisci con dettaglio tutte le scene
#7)monta
#8) carica su youtube

class Esercizio(ThreeDScene):
    def construct(self):
        aa=TexMobject(r"f(x,y)=x^2ye^{x-y}").scale(2)
        a=TexMobject(r"f(x,y)=x^2ye^{x-y}").scale(1.3).to_corner(UL,buff=1).shift(.5*DOWN)
        self.play(FadeInFromDown(aa))
        self.play(ReplacementTransform(aa,a))
        self.wait(1)
        passo1=TextMobject("Step 1: evaluate the gradient and set it = 0").set_color(BLUE,YELLOW).scale(1.3).to_edge(UP,buff=.5)
        self.wait()
        self.play(Write(passo1),run_time=3)
        grad=TexMobject(r"\vec{\nabla}f(x,y)=\begin{cases}2xye^{x-y}+x^2ye^{x-y}=0\\\\x^2e^{x-y}+x^2y\left(-e^{x-y}\right)=0\end{cases}").scale(1.3).to_edge(LEFT)
        self.play(FadeInFromDown(grad[0:8]))
        self.play(ShowCreation(grad[8]))
        self.play(GrowFromCenter(grad[9:14]))
        self.play(Write(grad[14:50]),run_time=3)
        self.wait(3)
        passo2=TextMobject("Step 2: factorize and delete the never null factors").set_color(BLUE,YELLOW).scale(1.3).to_edge(UP,buff=.5)
        self.play(ReplacementTransform(passo1,passo2))
        self.wait(1)
        self.play(grad[17:21].move_to,grad[14:17],grad[25:29].move_to,grad[14:17],grad[14:17].next_to,grad[21],LEFT,grad[29:31].next_to,grad[22:25],buff=SMALL_BUFF,run_time=2)
        par=TexMobject(r"\big(\big)").scale(1.3)
        par[0].next_to(grad[14:17],LEFT,buff=0)
        par[1].next_to(grad[22:25],buff=0).shift(.05*DOWN)
        self.play(FadeIn(par))
        x2=grad[22].copy()
        v1=VGroup(par,grad[14],grad[16],grad[21:23],grad[24],grad[29:31])
        grad14=grad[14].copy().shift(.1*DOWN)
        self.play(x2.move_to,grad14,grad[15].move_to,grad14,v1.shift,.5*RIGHT,FadeOut(grad[23]),run_time=2)
        self.wait(.5)#the x and the y
        v2=VGroup(grad[29:31],par[1])
        self.play(grad[24].next_to,grad14,grad[16].next_to,grad14,par[0].shift,.6*RIGHT,grad[14].shift,.6*RIGHT,v2.next_to,grad[22] ,buff=0,run_time=2)
        self.wait(1)
        meno=TexMobject(r"-").scale(1.3).move_to(grad[38])
        uno=TexMobject(r"\big(1\big)").scale(1.3)
        uno[0:2].next_to(grad[33:37],buff=0).shift(.1*DOWN)
        uno[2].next_to(grad[40],buff=0)
        self.play(grad[43:47].move_to,grad[33:37],grad[38:40].move_to,grad[31:33],FadeOut(grad[41]),FadeOut(grad[47]),ReplacementTransform(grad[37],meno),ReplacementTransform(grad[42],meno),FadeInAndShiftFromDirection(uno[0:2],LEFT),FadeIn(uno[2]),grad[48:50].next_to,uno[2],buff=0,run_time=3)
        null=TexMobject(r"\cancel{e^{x-y}}").move_to(grad[43:47])
        null[0:3].set_color(RED)
        null2=null.copy().move_to(grad[17:21])
        self.wait(2)
        self.play(ShowCreation(null[0:3]),ShowCreation(null2[0:3]))
        tr=VGroup(null[0:3],null2[0:3],grad[43:47],grad[33:37],grad[17:21],grad[25:29])
        self.play(FadeOut(tr))
        tr1=VGroup(x2,grad[15],par,grad[14],grad[21],grad[22],grad[29:31],grad[16],grad[24])
        tr2=VGroup(uno,meno,grad[40],grad[48:50])
        self.play(tr1.shift,1.3*LEFT,tr2.shift,LEFT)
        passo3=TextMobject("Step 3: divide the system into many tiny systems").set_color(BLUE,YELLOW).scale(1.3).to_edge(UP,buff=.5) #see every possible combination of the factors from the first and the second equation
        self.wait(1)
        self.play(ReplacementTransform(passo2,passo3))
        self.wait(1)
        ntr1=VGroup(par,grad[14],grad[21],grad[22])
        ntr2=VGroup(uno,meno,grad[40])
        a1=Arrow(x2,ntr2,buff=0).set_color(RED)
        a2=Arrow(grad[16],grad[31:33],buff=0).set_color(YELLOW)
        a3=Arrow(ntr1,ntr2,buff=0).set_color(BLUE)
        self.play(GrowArrow(a1),GrowArrow(a2),GrowArrow(a3))
        self.wait(1)
        s1=TexMobject(r"\begin{cases}x=0\\1-y=0\end{cases}").to_corner(DL).set_color(RED)
        s2=TexMobject(r"\begin{cases}y=0\\x^2=0\end{cases}").next_to(s1).set_color(YELLOW)
        s3=TexMobject(r"\begin{cases}2+x=0\\1-y=0\end{cases}").next_to(s2).set_color(BLUE)
        self.play(Write(s1),Write(s2),Write(s3),run_time=2)
        self.wait(1)
        punti=TexMobject(r"\Rightarrow\,(0,1)\,\,(0,0)\,\,(-2,1)").scale(1.3).next_to(s3)
        punti[1:6].set_color(RED)
        punti[6:11].set_color(YELLOW)
        punti[11:17].set_color(BLUE)
        self.play(GrowFromEdge(punti[0],LEFT))
        self.play(FadeInAndShiftFromDirection(punti[1:6],LEFT))
        self.play(FadeInAndShiftFromDirection(punti[6:11],LEFT))
        self.play(FadeInAndShiftFromDirection(punti[11:17],LEFT))
        self.wait()
        R=VGroup(x2,grad[31])
        r=SurroundingRectangle(R).set_color(PURPLE)
        self.play(ShowCreationThenDestruction(r),run_time=2)
        ale=Alex().rotate(PI,axis=Y_AXIS).move_to((10,0,0)).scale(.7)
        ale[4].set_color(BLUE)
        ale[2:4].shift(.03*DOWN)
        self.play(ale.move_to,(4,0,0))
        p=TextMobject(r"?").next_to(ale,UR,buff=0).scale(1.4)
        self.play(FadeIn(p))
        self.wait(1)
        self.play(Blink(ale))
        x0=TexMobject(r"(0,y)").set_color(PURPLE).move_to(punti[1:11]).scale(1.3)
        self.play(ReplacementTransform(punti[1:11],x0),ale[2:4].shift,.04*DOWN)
        self.wait(1)
        self.play(Blink(ale))
        self.wait(1)
        TUTTO=VGroup(grad[0:17],grad[21:23],grad[24],grad[29:33],grad[38:41],grad[48:50],punti[0],s1,s2,s3,a1,a2,a3,par,x2,meno,uno,p,a)
        self.play(FadeOut(TUTTO))
        alle=ale.copy().scale(.6).to_corner(UR).shift(DOWN)
        sol=VGroup(x0,punti[11:17])
        self.play(sol.move_to,ORIGIN,ReplacementTransform(ale,alle))
        self.play(Blink(alle))
        passo4=TextMobject("Step 4: find second derivatives and Hessians ").set_color(BLUE,YELLOW).scale(1.3).to_edge(UP,buff=.5) #see every possible combination of the factors from the first and the second equation
        self.wait(1)
        self.play(ReplacementTransform(passo3,passo4))
        self.wait()
        h1=TexMobject(r"H_{(-2,1)}=\left(\begin{matrix}-2e^{-3}&&0\\\\0&&-4e^{-3}\end{matrix}\right)").to_corner(DR)
        h1[10:15].set_color(RED)
        h1[15].set_color(YELLOW)
        h1[16].set_color(YELLOW)
        h1[17:22].set_color(BLUE)
        h2=TexMobject(r"H_{(0,y)}=\left(\begin{matrix}0&&0\\\\0&&0\end{matrix}\right)").to_corner(DL)
        h2[9].set_color(RED)
        h2[10].set_color(YELLOW)
        h2[11].set_color(YELLOW)
        h2[12].set_color(BLUE)
        self.play(FadeInFromDown(h2[0:6]),FadeInFromDown(h1[0:7]))
        self.play(ShowCreation(h1[7]),ShowCreation(h2[6]))
        self.play(Write(h1[8:10]),Write(h1[10:22]),Write(h1[22:24]),Write(h2[7:15]))
        self.wait()
        self.play(Blink(alle))
        passo5=TextMobject("Step 5: find the eigenvalues of the Hessians").set_color(BLUE,YELLOW).scale(1.3).to_edge(UP,buff=.5)
        self.play(ReplacementTransform(passo4,passo5))
        self.wait()
        self.play(FadeOut(sol),h1.move_to,(-3.2,0,0))
        aut1=TexMobject(r"\Rightarrow \lambda_1=-2,\lambda_2=-4").next_to(h1)
        self.play(FadeInAndShiftFromDirection(aut1[0],LEFT))
        self.play(Write(aut1[1:12]))
        r1=SurroundingRectangle(aut1[1:6]).set_color(PURPLE)
        r2=SurroundingRectangle(aut1[7:12]).set_color(PURPLE)
        self.play(ShowCreationThenDestruction(r1),ShowCreationThenDestruction(r2),Blink(alle))
        self.wait()
        maxim=TextMobject("(-2,1) is a Maximum Point").scale(.8).set_color(PURPLE).next_to(aut1[1:12],UP)
        self.play(FadeInFromDown(maxim))
        aut2=TexMobject(r"\Rightarrow \lambda_1=\lambda_2=0").next_to(h2)
        self.wait()
        self.play(FadeInAndShiftFromDirection(aut2[0],LEFT))
        self.play(Write(aut2[1:8]))
        inc=TextMobject("Hessian Method is inconclusive").scale(.8).set_color(BLUE).next_to(aut2)
        self.play(Write(inc))
        self.wait()
        self.play(Blink(alle))
        self.wait()
        
class Esercizio2(ThreeDScene):
    def construct(self):
        aa=TexMobject(r"f(x,y)=x^2ye^{x-y}").scale(2)
        a=TexMobject(r"f(x,y)=x^2ye^{x-y}").scale(1.3).to_corner(UL,buff=1).shift(.5*DOWN)
        b=TexMobject(r"c=f(0,y)=0").scale(1.3).next_to(a,DOWN,buff=2).set_color(YELLOW)
        self.play(FadeInFromDown(aa))
        self.play(ReplacementTransform(aa,a))
        hat=TexMobject(r"\bar{f}").scale(1.3).move_to(a[0])
        mo=TexMobject(r"\geq 0").scale(1.3).next_to(a)
        self.wait(1)
        self.play(Transform(a[0],hat))
        self.play(FadeInFromDown(b))
        self.wait()
        self.play(FadeOutAndShift(b,UP))
        self.play(ShowCreation(mo[0]))
        self.play(Write(mo[1]))
        self.wait()
        can=TexMobject(r"\cancel{e^{x-y}}").scale(1.3).move_to(a[10:14])
        can[0:3].set_color(RED)
        self.play(FadeOutAndShift(a[0:7],LEFT))
        self.play(ShowCreation(can[0:3]))
        self.play(FadeOut(can[0:3]),FadeOut(a[10:14]),mo.next_to,a[7:10])
        self.wait()
        can2=TexMobject(r"\cancel{x^2}").scale(1.3).move_to(a[7:9])
        can2[0:2].set_color(RED)
        self.play(ShowCreation(can2[0:2]))
        t=VGroup(mo,a[9])
        self.play(FadeOut(can2[0:2]),FadeOut(a[7:9]),t.next_to,a[6])
        axes=ThreeDAxes()
        self.play(ShowCreation(axes),t.to_corner,UL,buff=MED_SMALL_BUFF)
        self.wait()
        s1=Square(side_length=4).set_fill(BLUE,opacity=.4).next_to((0,0,0),UR,buff=0)
        s2=Square(side_length=4).set_fill(BLUE,opacity=.4).next_to((0,0,0),UL,buff=0)
        s3=Square(side_length=4).set_fill(RED,opacity=.4).next_to((0,0,0),DR,buff=0)
        s4=Square(side_length=4).set_fill(RED,opacity=.4).next_to((0,0,0),DL,buff=0)
        S=VGroup(s1,s2,s3,s4)
        self.play(FadeIn(S))
        self.wait()
        dotti=VGroup(*list(Dot((0,-4+i/2,0)).set_color(YELLOW) for i in range(0,16)))
        for i in range(1,16):
         self.play(FadeInFromLarge(dotti[i],5),run_time=.1)
        self.wait()
        self.play( *[Broadcast(lag_ratio=.7,focal_point=np.array([0,-4+i/2,0]),
        big_radius=.5,
        run_time=1, color=YELLOW) for i in range(1,8)])
        self.wait()
        bra=Brace(mobject=dotti[1:8],direction=RIGHT)
        tbra=bra.get_text(r"Maximum Points").scale(.8).shift(.4*LEFT)
        self.play(GrowFromCenter(bra))
        self.play(Write(tbra))
        self.play(FadeToColor(dotti[1:8],RED),run_time=2)
        bra2=Brace(mobject=dotti[9:16],direction=LEFT)
        tbra2=bra2.get_text(r"Minimum Points").scale(.8).shift(.4*RIGHT)
        self.play( *[Broadcast(lag_ratio=.7,focal_point=np.array([0,-4+i/2,0]),
        big_radius=.5,
        run_time=1, color=YELLOW) for i in range(9,16)])
        self.play(GrowFromCenter(bra2))
        self.play(Write(tbra2))
        self.play(FadeToColor(dotti[9:16],BLUE),run_time=2)
        self.play(Broadcast(lag_ratio=.7,focal_point=np.array([0,0,0]),
        big_radius=.5,
        run_time=3, color=YELLOW))
        sad=TextMobject(r"Saddle Point").scale(.8).move_to((2,1,0))
        ar=Arrow(sad,dotti[8]).set_color(GREEN)
        self.play(GrowArrow(ar))
        self.play(Write(sad))
        self.wait()

class prova(ThreeDScene):
    CONFIG = {
        "rows":8,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
    }
    def construct(self):
     self.move_camera(phi=70*DEGREES, theta=20*DEGREES)
     self.begin_ambient_camera_rotation(rate=0.02)
     axes = ThreeDAxes()
     grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
     grilla2=grilla.copy().next_to(grilla,UP,buff=0)
     grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
     grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
     grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
     grilla6=grilla.copy().next_to(grilla,RIGHT+DOWN,buff=0)
     grilla7=NumberPlane(width=24,height=4,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke).next_to(grilla,RIGHT,buff=0)
     grilla8=grilla.copy().next_to(grilla,LEFT,buff=0)
     self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8)
     surface=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                3*np.exp(-(v-1)**2/2-(u-1)**2/2)  #2*np.exp(-(v-1)**2/2)
            ]),v_min=-2,v_max=4,u_min=-3,u_max=3,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color_by_gradient(GREEN,BLUE)
     surface2=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                2*np.exp(-(v-1)**2/2)  #2*np.exp(-(v-1)**2/2)
            ]),v_min=-2,v_max=4,u_min=-3,u_max=3,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color_by_gradient(GREEN,BLUE)
     self.play(ShowCreation(surface))
     l1=DashedLine((1,0,0),(1,.95,0)).set_color(YELLOW)
     l2=DashedLine((0,1,0),(.95,1,0)).set_color(YELLOW)
     l3=DashedLine((1,1,0.05),(1,1,3)).set_color(YELLOW)
     dot=Dot((1,1,0)).set_color(RED).scale(.7)
     self.play(ShowCreation(l1),ShowCreation(l2),FadeIn(dot))
     self.play(ShowCreation(l3))
     self.wait(1)
     self.play(FadeOut(l1),FadeOut(l2),FadeOut(l3),FadeOut(dot))
     self.play(Transform(surface,surface2))
     l=ParametricFunction(
                lambda u : np.array([
                u,
                1,
                0
            ]),color=RED,t_min=-3,t_max=3,
            )
     dl=DashedLine((-3,1,0),(-3,1,2)).set_color(YELLOW)       
     self.play(ShowCreation(dl))
     self.play(ShowCreation(l),dl.shift,6*RIGHT,run_time=4,rate_func=linear)
     self.wait(2)

class intr(Scene):
    def construct(self):
        o=TextMobject(r"OPTIMIZATION").scale(3).set_color(YELLOW)
        self.play(FadeInFromDown(o))
        self.wait()
        s=Square().set_color(WHITE).scale(1.2)
        A=SVGMobject("arrow").set_color(WHITE).next_to(s,LEFT).scale(.8)
        A2=SVGMobject("arrow").to_edge(RIGHT).set_color(WHITE).next_to(s).scale(.8)
        t=TextMobject(r"inputs \\ (variables)").next_to(A,LEFT)
        t2=TextMobject(r"best possible \\ outputs").next_to(A2)
        t3=TextMobject(f"function").move_to(s)
        self.play(FadeOutAndShiftDown(o),ShowCreation(s),DrawBorderThenFill(A),DrawBorderThenFill(A2))
        self.wait()
        self.play(Write(t),Write(t2),Write(t3))
        self.wait()




class HNullo1(GraphScene):
    CONFIG = {
        "x_min": -2,
        "x_max": 8,
        "y_min": -3,
        "y_max": 4,
        "rows":20,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
     }
    def construct(self):
        grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        self.add(grilla)
        self.setup_axes(animate=False)
        v=VGroup(self.axes,self.x_axis,self.y_axis).shift(2*UP+2*RIGHT)
        funz=self.get_graph(lambda x: np.sin(x)+2,x_min=0,x_max=2*PI,color=GREEN)
        f=TexMobject(r"f(x)").scale(1.2).to_corner(UL)
        f[0].set_color(GREEN)
        fb=TexMobject(r"\bar{f}(x)\equiv f(x)-c").scale(1.2).to_corner(UL,buff=3*SMALL_BUFF)
        fb[0:2].set_color(RED)
        fb[6].set_color(GREEN)
        fb[11].set_color(YELLOW)
        xv=self.get_vertical_line_to_graph(PI/2,funz,line_class=DashedLine,color=YELLOW)
        xv2=self.get_vertical_line_to_graph(3*PI/2,funz,line_class=DashedLine,color=YELLOW)
        x2=TexMobject(r"x_2").next_to(xv2,DOWN).set_color(PINK)
        x=TexMobject(r"x_1").next_to(xv,DOWN).set_color(PURPLE)
        funzb=self.get_graph(lambda x: np.sin(x)-1,x_min=0,x_max=2*PI,color=RED)
        funzc=self.get_graph(lambda x: np.sin(x)+1,x_min=0,x_max=2*PI,color=BLUE)
        self.add(v)
        self.play(ShowCreation(funz),FadeIn(f))
        self.wait(1)
        self.play(ShowCreation(xv),FadeIn(x))
        self.wait()
        yv=DashedLine(self.coords_to_point(PI/2,3),self.coords_to_point(0,3)).set_color(YELLOW)
        yv2=DashedLine(self.coords_to_point(PI/2,0),self.coords_to_point(0,0)).set_color(YELLOW)
        yv3=DashedLine(self.coords_to_point(3*PI/2,1),self.coords_to_point(0,1)).set_color(YELLOW)
        yv4=DashedLine(self.coords_to_point(3*PI/2,0),self.coords_to_point(0,0)).set_color(YELLOW)
        y=TexMobject(r"f(x_1)").next_to(yv,LEFT)
        y[0].set_color(GREEN)
        y[2:4].set_color(PURPLE)
        c=TexMobject(r"c").move_to(y).set_color(YELLOW)
        c.add_updater(lambda m: m.next_to(yv,LEFT))
        y2=TexMobject(r"f(x_2)").next_to(yv3,LEFT)
        y2[0].set_color(GREEN)
        y2[2:4].set_color(PINK)
        c2=TexMobject(r"c").move_to(yv3).set_color(YELLOW)
        c2.add_updater(lambda m: m.next_to(yv3,LEFT))
        self.play(ShowCreation(yv),FadeIn(y))
        self.play(ReplacementTransform(y,c))
        self.wait()
        ale=Alex().scale(.5).move_to((10,1,0)).rotate(PI,axis=Y_AXIS)
        ale[4].set_color(BLUE)
        ale[2:4].shift(.02*DOWN)
        self.play(ale.move_to,(5,1,0))
        mask=SVGMobject("mask").move_to(ale).scale(.18).shift(.4*UP+.05*LEFT).set_color(PURPLE)
        pi=TextMobject(r"?").next_to(ale,UP+RIGHT,buff=0)
        self.wait()
        self.play(Blink(ale))
        self.wait()
        self.play(FadeInFrom(mask,UP),FadeIn(pi))
        self.wait()
        bubble=SpeechBubble().scale(.5).next_to(ale,UL,buff=0).rotate(PI,axis=Y_AXIS)
        palabras=TextMobject(r"If i shift the function by \textbf{\textit{c}}, \\ will the extreme be \\ in a + or - region?").move_to(bubble).scale(.5).shift(.2*UP)
        palabras[21].set_color(YELLOW)
        palabras[42].set_color(BLUE)
        palabras[45].set_color(RED)
        self.play(FadeOut(pi),ShowCreation(bubble),Write(palabras),run_time=2)
        self.wait(2)
        self.play(ReplacementTransform(f,fb))
        self.play(FadeOut(bubble),FadeOut(palabras))
        self.wait()
        self.play(ReplacementTransform(yv,yv2),TransformFromCopy(funz,funzb),Uncreate(xv),run_time=4)
        self.play(FadeOut(c))
        fb2=TexMobject(r"\bar{f}(x_1)\equiv f(x_1)-c").scale(1.2).to_corner(UL,buff=3*SMALL_BUFF).shift(DOWN)
        fb2[0:2].set_color(RED)
        fb2[3:5].set_color(PURPLE)
        fb2[7].set_color(GREEN)
        fb2[9:11].set_color(PURPLE)
        fb2[13].set_color(YELLOW)
        self.wait(1)
        cc=fb2[13].copy().move_to(fb2[7])
        self.play(TransformFromCopy(fb,fb2))
        self.play(ReplacementTransform(fb2[7:12],cc),fb2[12:14].next_to,cc,buff=3*SMALL_BUFF)
        u0=TexMobject(r"=0").next_to(fb2[13],buff=SMALL_BUFF)
        self.play(ShowCreation(u0[0]))
        self.play(Write(u0[1]))
        self.wait(1)
        funzbp=self.get_graph(lambda x: np.sin(x)-1,x_min=0,x_max=PI,color=RED)
        dot=Dot(self.coords_to_point(0,-1)).scale(.7).set_color(RED)
        self.play(FadeInFromLarge(dot,10))
        fb3=TexMobject(r"\bar{f}=").scale(.7)
        fb3.add_updater(lambda m: m.next_to(dot,UP+LEFT))
        fb3[0:2].set_color(RED)
        time1=ValueTracker(0)
        ft=lambda x: np.sin(x.get_value())-1
        time2=ValueTracker(ft(time1))
        a = DecimalNumber(time2.get_value()).add_updater(lambda v: v.set_value(ft(time1)).scale(.7)).set_color(RED).next_to(fb3,buff=0)
        a.add_updater(lambda m: m.next_to(fb3,buff=SMALL_BUFF))
        self.play(FadeInFromDown(fb3),FadeInFromDown(a))
        self.play(time1.set_value,PI,MoveAlongPath(dot,funzbp),rate_func=there_and_back,run_time=4)
        xmax=TexMobject(r"x_{\text{max}}").set_color(PURPLE).next_to(x,UP,buff=.5)
        self.play(TransformFromCopy(x,xmax),FadeOut(a),FadeOut(fb3),FadeOut(dot))
        self.wait(1)
        self.play(ShowCreation(xv2),FadeIn(x2))
        self.play(ShowCreation(yv3),FadeIn(y2))
        self.play(ReplacementTransform(y2,c2))
        self.play(ReplacementTransform(yv3,yv4),TransformFromCopy(funz,funzc),Uncreate(xv2),run_time=4)
        self.wait(1)
        funzcp=self.get_graph(lambda x: np.sin(x)+1,x_min=PI,x_max=2*PI,color=BLUE)
        dot2=Dot(self.coords_to_point(PI,1)).scale(.7).set_color(BLUE)
        self.play(FadeInFromLarge(dot2,10))
        fc3=TexMobject(r"\bar{f}=").scale(.7)
        fc3.add_updater(lambda m: m.next_to(dot2,UP+LEFT))
        fc3[0:2].set_color(BLUE)
        time11=ValueTracker(PI)
        ft1=lambda x: np.sin(x.get_value())+1
        time22=ValueTracker(ft1(time11))
        b = DecimalNumber(time22.get_value()).add_updater(lambda v: v.set_value(ft1(time11)).scale(.7)).set_color(BLUE).next_to(fc3,buff=0)
        b.add_updater(lambda m: m.next_to(fc3,buff=SMALL_BUFF))
        self.play(FadeInFromDown(fc3),FadeInFromDown(b))
        self.play(time11.set_value,2*PI,MoveAlongPath(dot2,funzcp),rate_func=there_and_back,run_time=4)
        xmin=TexMobject(r"x_{\text{min}}").set_color(PINK).next_to(x2,UP,buff=.5)
        self.play(TransformFromCopy(x2,xmin),FadeOut(b),FadeOut(fc3))
        self.play(FadeOut(dot2))
        self.wait(1)
        self.play(FadeOutAndShift(mask,UP))
        bubble2=SpeechBubble().scale(.5).next_to(ale,UL,buff=0).rotate(PI,axis=Y_AXIS)
        palabras2=TextMobject(r"Nailed it!").move_to(bubble).shift(.27*UP)
        self.play(Blink(ale))
        self.play(ale[2:4].shift,.03*DOWN)
        self.play(ShowCreation(bubble2),Write(palabras2))
        self.wait()
        self.play(Blink(ale))
        self.wait()

class H0(ThreeDScene):
    CONFIG = {
        "rows":8,
        "columns":24,
        "height": FRAME_Y_RADIUS*2,
        "width": 24,
        "grid_stroke":0.1,
        "grid_color":WHITE,
        "axis_color":RED,
        "axis_stroke":2,
        "show_points":False,
        "point_radius":0,
        "labels_scale":0.5,
        "labels_buff":0,
        "number_decimals":2
    }
    def construct(self):
     self.move_camera(phi=70*DEGREES, theta=20*DEGREES)
     self.begin_ambient_camera_rotation(rate=0.01)
     axes = ThreeDAxes(x_max=4.5)
     grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
     grilla2=grilla.copy().next_to(grilla,UP,buff=0)
     grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
     grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
     grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
     #grilla6=grilla.copy().next_to(grilla,RIGHT+DOWN,buff=0)
     grilla7=NumberPlane(width=24,height=4,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke).next_to(grilla,RIGHT,buff=0)
     grilla8=grilla.copy().next_to(grilla,LEFT,buff=0)
     self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla7,grilla8)
     surface=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(u)*np.sin(v)+2
            ]),v_min=-PI,v_max=PI,u_min=-PI,u_max=PI,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7).set_color_by_gradient(BLUE,GREEN)
     p1=Sphere(radius=.03).set_color(RED).move_to((0,PI/2,3))
     p2=p1.copy().move_to((PI/2,0,2))
     p3=p1.copy().move_to((0,-PI/2,1))
     self.add(surface,p1,p2,p3)
     P=VGroup(surface,p1,p2,p3)
     self.wait(.5)
     self.play(P.shift,-3*Z_AXIS,rate_func=linear,run_time=2)
     self.play( 
        Broadcast(lag_ratio=.7,focal_point=np.array([0,PI/2,0]),
        big_radius=1,
        run_time=1, color=GREEN))
     #intorno1=ParametricSurface(
     #           lambda u,v : np.array([
     #           u,
     #           v,
     #           np.cos(u)*np.sin(v)-1
     #       ]),v_min=PI/2-1,v_max=PI/2+1,u_min=-1,u_max=1,checkboard_colors=[BLUE_D,BLUE_E],
     #       resolution=(30, 63)).fade(.7).set_color(BLUE)
     #self.play(ShowCreation(intorno1))
     M=TextMobject(r"Maximum point").scale(.5).set_color(RED).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).move_to(p1).shift(.2*Z_AXIS)
     self.play(Write(M))
     self.wait(1)
     self.move_camera(phi=70*DEGREES, theta=-20*DEGREES)
     self.play(P.shift,+2*Z_AXIS,rate_func=linear,run_time=2)
     self.play( 
        Broadcast(lag_ratio=.7,focal_point=np.array([0,-PI/2,0]),
        big_radius=1,
        run_time=1, color=GREEN))
     m=TextMobject(r"Minimum point").scale(.5).set_color(BLUE).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).move_to(p3).shift(-.2*Z_AXIS)
     self.play(Write(m))
     self.wait(1)
     self.move_camera(phi=70*DEGREES, theta=0*DEGREES)
     self.play(P.shift,-Z_AXIS,rate_func=linear,run_time=2)
     self.play( 
        Broadcast(lag_ratio=.7,focal_point=np.array([PI/2,0,0]),
        big_radius=1,
        run_time=1, color=GREEN))
     s=TextMobject(r"Saddle point").scale(.5).set_color_by_gradient(RED,BLUE).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS).move_to(p2).shift(.2*Z_AXIS)
     self.play(Write(s))
     self.wait(1)
     
     
     #self.move_camera(phi=0*DEGREES, theta=90*DEGREES)
     #self.stop_ambient_camera_rotation()
     #self.wait(1)
     
