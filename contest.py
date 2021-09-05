from big_ol_pile_of_manim_imports import *
import itertools as it
import array as arr
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

class Curve11(ThreeDScene): #ricomputa quando hai tempo (METTI ESEMPIO PER f(x) e f(x,y) e fai vedere linee verticai su asse z???)
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
           axes = ThreeDAxes()
           grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
           grilla2=grilla.copy().next_to(grilla,UP,buff=0)
           grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
           grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
           grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
           grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
           grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
           self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
           self.play(Write(axes),Write(grilla))
           funz=ParametricFunction(
                lambda u : np.array([
                u,
                0.4*(u**3+u**2-6*u),
                0
            ]),color=RED,t_min=-4,t_max=4,
            )
           funzdot=VGroup(*list(Dot((i/5,0.4*((i/5)**3+(i/5)**2-6*(i/5)),0)).scale(.5) for i in range (-20,20))).set_color(RED)
           funzdot2=VGroup(*list(Dot((i/10,0.4*((i/10)**3+(i/10)**2-6*(i/10)),0)).scale(.5) for i in range (-40,40))).set_color(RED)
           funzdot3=VGroup(*list(Dot((i/20,0.4*((i/20)**3+(i/20)**2-6*(i/20)),0)).scale(.5) for i in range (-80,80))).set_color(RED)
           funzdot4=VGroup(*list(Dot((i/40,0.4*((i/40)**3+(i/40)**2-6*(i/40)),0)).scale(.5) for i in range (-160,160))).set_color(RED)
           funz
           funzz=ParametricFunction(
                lambda u : np.array([
                u,
                0.4*(u**3+u**2-6*u),
                0
            ]),color=RED,t_min=-4,t_max=2.2,
            )
           y=TexMobject(r"y=f(x)").scale(2).to_corner(UL)
           self.play(Write(y))
           self.wait(1)
           self.play(ShowCreation(funzdot,run_time=2))
           self.wait(1)
           self.play(ReplacementTransform(funzdot,funzdot2))
           self.play(ReplacementTransform(funzdot2,funzdot3))
           self.play(ReplacementTransform(funzdot3,funzdot4))
           self.wait(2)
           self.play(FadeIn(funz),FadeOut(funzdot4))
           self.wait(2)
           dot=Dot((-3,
                0.4*((-4)**3+(-4)**2-6*(-4)),
                0)).set_color(RED).scale(.7)
           inte=TexMobject(r"\int_a^bf(x)dx").move_to(y)
           inte[1].set_color(GREEN)
           inte[2].set_color(PURPLE)
           self.play(MoveAlongPath(dot,funzz,run_time=4))
           dx=DashedLine((2.2,0,0),(2.2,0.4*((2.2)**3+(2.2)**2-6*(2.2)-0.1),0)).set_color(YELLOW)
           dy=DashedLine((2.2-0.1,0.4*((2.2)**3+(2.2)**2-6*(2.2)),0),(0,0.4*((2.2)**3+(2.2)**2-6*(2.2)),0)).set_color(YELLOW)
           fx=TexMobject(r"f(x)").next_to(dx,RIGHT).scale(.7)
           x=TexMobject(r"x").next_to(dy,UP).scale(.7)
           self.play(ShowCreation(dy),FadeIn(x))
           self.play(ShowCreation(dx),FadeIn(fx))
           self.wait(1)
           self.play(FadeOut(dx),FadeOut(dy),FadeOut(dot),FadeOut(x),FadeOut(fx))
           self.wait(1)
           
class Curve1(ThreeDScene): #ricomputa quando hai tempo (METTI ESEMPIO PER f(x) e f(x,y) e fai vedere linee verticai su asse z???)
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
           axes = ThreeDAxes()
           grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
           grilla2=grilla.copy().next_to(grilla,UP,buff=0)
           grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
           grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
           grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
           grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
           grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
           self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
           self.play(Write(axes),Write(grilla))
           funz=ParametricFunction(
                lambda u : np.array([
                u,
                0.4*(u**3+u**2-6*u),
                0
            ]),color=RED,t_min=-4,t_max=4,
            )
           funzdot=VGroup(*list(Dot((i/5,0.4*((i/5)**3+(i/5)**2-6*(i/5)),0)).scale(.5) for i in range (-20,20))).set_color(RED)
           funzdot2=VGroup(*list(Dot((i/10,0.4*((i/10)**3+(i/10)**2-6*(i/10)),0)).scale(.5) for i in range (-40,40))).set_color(RED)
           funzdot3=VGroup(*list(Dot((i/20,0.4*((i/20)**3+(i/20)**2-6*(i/20)),0)).scale(.5) for i in range (-80,80))).set_color(RED)
           funzdot4=VGroup(*list(Dot((i/40,0.4*((i/40)**3+(i/40)**2-6*(i/40)),0)).scale(.5) for i in range (-160,160))).set_color(RED)
           funz
           funzz=ParametricFunction(
                lambda u : np.array([
                u,
                0.4*(u**3+u**2-6*u),
                0
            ]),color=RED,t_min=-4,t_max=2.2,
            )
           y=TexMobject(r"y=f(x)").scale(2).to_corner(UL)
           self.play(Write(y))
           self.wait(1)
           self.play(ShowCreation(funzdot,run_time=2))
           self.wait(1)
           self.play(ReplacementTransform(funzdot,funzdot2))
           self.play(ReplacementTransform(funzdot2,funzdot3))
           self.play(ReplacementTransform(funzdot3,funzdot4))
           self.wait(2)
           self.play(FadeIn(funz),FadeOut(funzdot4))
           self.wait(2)
           dot=Dot((-3,
                0.4*((-4)**3+(-4)**2-6*(-4)),
                0)).set_color(RED).scale(.7)
           inte=TexMobject(r"\int_a^bf(x)dx").move_to(y)
           inte[1].set_color(GREEN)
           inte[2].set_color(PURPLE)
           self.play(MoveAlongPath(dot,funzz,run_time=4))
           dx=DashedLine((2.2,0,0),(2.2,0.4*((2.2)**3+(2.2)**2-6*(2.2)-0.1),0)).set_color(YELLOW)
           dy=DashedLine((2.2-0.1,0.4*((2.2)**3+(2.2)**2-6*(2.2)),0),(0,0.4*((2.2)**3+(2.2)**2-6*(2.2)),0)).set_color(YELLOW)
           fx=TexMobject(r"f(x)").next_to(dx,RIGHT).scale(.7)
           x=TexMobject(r"x").next_to(dy,UP).scale(.7)
           self.play(ShowCreation(dy),FadeIn(x))
           self.play(ShowCreation(dx),FadeIn(fx))
           self.wait(1)
           self.play(FadeOut(dx),FadeOut(dy),FadeOut(dot),FadeOut(x),FadeOut(fx))
           self.wait(1)
           r=VGroup(*list(Rectangle(width=3/7, height=0.4*((-3*i/7)**3+(-3*i/7)**2-6*(-3*(i/7))), color=BLUE, stroke_width=0) for i in range (0,8)))
           rr=VGroup(*list(Rectangle(width=3/14, height=0.4*((-3*i/14)**3+(-3*i/14)**2-6*(-3*(i/14))), color=BLUE, stroke_width=0) for i in range (0,15)))
           rrr=VGroup(*list(Rectangle(width=3/29, height=0.4*((-3*i/29)**3+(-3*i/29)**2-6*(-3*(i/29))), color=BLUE, stroke_width=0) for i in range (0,30)))
           rrrr=VGroup(*list(Rectangle(width=3/59, height=0.4*((-3*i/59)**3+(-3*i/59)**2-6*(-3*(i/59))), color=BLUE, stroke_width=0) for i in range (0,60)))
           for i in range(0,8):
               r[i].move_to((-3*i/7,0.4*((-3*i/7)**3+(-3*i/7)**2-6*(-3*i/7))/2,0))
               r[i].set_fill(BLUE, opacity=0.5)
           for i in range(0,15):
               rr[i].move_to((-3*i/14,0.4*((-3*i/14)**3+(-3*i/14)**2-6*(-3*(i/14)))/2,0))
               rr[i].set_fill(BLUE, opacity=0.5)
           for i in range(0,30):
               rrr[i].move_to((-3*i/29,0.4*((-3*i/29)**3+(-3*i/29)**2-6*(-3*(i/29)))/2,0))
               rrr[i].set_fill(BLUE, opacity=0.5)
           for i in range(0,60):
               rrrr[i].move_to((-3*i/59,0.4*((-3*i/59)**3+(-3*i/59)**2-6*(-3*(i/59)))/2,0))
               rrrr[i].set_fill(BLUE, opacity=0.5)
           r0=VGroup(*list(Rectangle(width=3/7, height=0.01+0*i, color=BLUE, stroke_width=0) for i in range (0,8)))
           for i in range(0,8):
               r0[i].move_to((-3*i/7,0.005,0))
               r0[i].set_fill(BLUE, opacity=0.5)   
           r.set_color_by_gradient(ORANGE,PURPLE)
           rr.set_color_by_gradient(ORANGE,PURPLE)
           rrr.set_color_by_gradient(ORANGE,PURPLE)
           rrrr.set_color_by_gradient(ORANGE,PURPLE)
           r0.set_color_by_gradient(ORANGE,PURPLE)
           self.play(FadeIn(rrrr))
           self.wait(1)
           self.play(FadeOut(rrrr))
           self.wait(1)
           self.play(ReplacementTransform(r0,r))
           self.wait(1)
           brace2d=Brace(mobject=r[4], direction=DOWN, buff=0)
           text2d=brace2d.get_tex(r"\Delta x")
           brace2d2=Brace(mobject=r[4], direction=LEFT, buff=0.1)
           text2d2=brace2d2.get_tex(r"f(x)")
           self.play(GrowFromCenter(brace2d),FadeIn(text2d))
           self.wait(1)
           self.play(GrowFromCenter(brace2d2),FadeIn(text2d2))
           brace2d3=Brace(mobject=rr[8], direction=DOWN, buff=0)
           text2d3=brace2d3.get_tex(r"\Delta x")
           self.wait(1)
           self.play(FadeOut(brace2d2),FadeOut(text2d2))
           self.play(ReplacementTransform(r,rr),ReplacementTransform(brace2d,brace2d3),ReplacementTransform(text2d,text2d3))
           brace2d5=Brace(mobject=rrr[16], direction=DOWN, buff=0)
           text2d5=brace2d5.get_tex(r"\Delta x")
           self.play(ReplacementTransform(rr,rrr),ReplacementTransform(brace2d3,brace2d5),ReplacementTransform(text2d3,text2d5))
           brace2d7=Brace(mobject=rrrr[32], direction=DOWN, buff=0)
           text2d7=brace2d7.get_tex(r"\Delta x")
           self.play(ReplacementTransform(rrr,rrrr),ReplacementTransform(brace2d5,brace2d7),ReplacementTransform(text2d5,text2d7))
           self.wait(1)
           text2d9=TexMobject(r"dx").move_to(text2d7)
           self.play(ReplacementTransform(text2d7,text2d9))
           self.wait(1)
           L=Line((-2.98,0,0),(-0.02,0,0)).set_color(PINK)
           a=TexMobject(r"a").move_to((-2.9,-0.2,0)).set_color(PURPLE)
           b=TexMobject(r"b").move_to((-0.1,-0.2,0)).set_color(GREEN)
           self.play(ShowCreation(L,run_time=3))
           self.play(FadeInFromLarge(a,5))
           self.play(FadeInFromLarge(b,5))
           self.play(FadeOut(y))
           self.play(FadeInFromDown(inte))
           self.wait(1)
           self.play(FadeOut(text2d9),FadeOut(text2d7),FadeOut(brace2d7))
           self.play(inte[0].scale,1.5)
           self.play(inte[0].scale,.7)
           self.wait(1)
           for i in range(0,60):
             rrrr[i].save_state()
           for i in range(0,60):
             self.play(rrrr[59-i].set_color,YELLOW,run_time=0.004)
             self.play(Restore(rrrr[59-i],run_time=0.004))
           self.wait(1)
           plane=ParametricSurface(
                lambda u,v : np.array([
                u,
                v,
                np.cos(u)+np.sin(v)+2
            ]),v_min=-4,v_max=4,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7)

         #SCENA 3D

           self.move_camera(phi=70*DEGREES,theta=-37*DEGREES)
           self.begin_ambient_camera_rotation(rate=0.007)
           self.play(FadeOut(funz),FadeOut(rrrr),FadeOut(inte),FadeOut(a),FadeOut(b),FadeOut(L))
           p0=VGroup(*list(VGroup(*list(Prism(dimensions=[1/5, 1/5, .01+0*i*j], stroke_width=0) for i in range (0,11))) for j in range (0,11)))
           for i in range (0,11):
             for j in range (0,11):
                   p0[j][i].move_to((1+i/5,1+j/5,.005))
           p0.set_color_by_gradient(ORANGE,PURPLE)
           p=VGroup(*list(VGroup(*list(Prism(dimensions=[1/5, 1/5, np.cos(1+i/5)*np.sin(1+j/5)+2], stroke_width=0) for i in range (0,11))) for j in range (0,11)))
           for i in range (0,11):
             for j in range (0,11): 
                   p[j][i].move_to((1+i/5,1+j/5,(np.cos(1+i/5)*np.sin(1+j/5)+2)/2))
           p.set_color_by_gradient(ORANGE,PURPLE)
           pp=VGroup(*list(VGroup(*list(Prism(dimensions=[1/10, 1/10, np.cos(1+i/10)*np.sin(1+j/10)+2], stroke_width=0) for i in range (0,21))) for j in range (0,21)))
           for i in range (0,21):
             for j in range (0,21):
                  pp[j][i].move_to((1+i/10,1+j/10,(np.cos(1+i/10)*np.sin(1+j/10)+2)/2))
           pp.set_color_by_gradient(ORANGE,PURPLE)
           ppp=VGroup(*list(VGroup(*list(Prism(dimensions=[1/20, 1/20, np.cos(1+i/20)*np.sin(1+j/20)+2], stroke_width=0) for i in range (0,41))) for j in range (0,41)))
           for i in range (0,41):
             for j in range (0,41):
                   ppp[j][i].move_to((1+i/20,1+j/20,(np.cos(1+i/20)*np.sin(1+j/20)+2)/2))
           ppp.set_color_by_gradient(ORANGE,PURPLE)
           pppp=VGroup(*list(VGroup(*list(Prism(dimensions=[1/40, 1/40, np.cos(1+i/40)*np.sin(1+j/40)+2], stroke_width=0) for i in range (0,81))) for j in range (0,81)))
           for i in range (0,81):
             for j in range (0,81):
                   pppp[j][i].move_to((1+i/40,1+j/40,(np.cos(1+i/40)*np.sin(1+j/40)+2)/2))
           pppp.set_color_by_gradient(ORANGE,PURPLE)
           self.wait(1)
           planedot=VGroup(*list(VGroup(*list(Dot((1+i/3,1+j/3,0)).scale(.2) for i in range (0,11))) for j in range (0,11))).set_color(BLUE)
           L3D=DashedLine((1+5/3,1+5/3,0),(1+5/3,1+5/3,np.cos(1+5/3)*np.sin(1+5/3)+2)).set_color(YELLOW)
           H=TexMobject(r"f(x,y)").scale(.5).next_to(L3D,LEFT).rotate(PI/2,axis=Y_AXIS)
           H.rotate(PI/2,axis=X_AXIS)
           H.shift(.2*Z_AXIS+0.2*DOWN)
           LX3D=DashedLine((2+5/3,0,0),(2+5/3,1+5/3-3.5,0)).set_color(YELLOW)
           HY=TexMobject(r"y").next_to(LX3D).scale(.7)
           LY3D=DashedLine((0,1+5/3-3.5,0),(1+5/3+1,1+5/3-3.5,0)).set_color(YELLOW)
           HX=TexMobject(r"x").next_to(LY3D,DOWN).scale(.7)
           HX.shift(.7*RIGHT)
           tutto=VGroup(p0,p,pp,ppp,pppp,plane,dx,dy,planedot,L3D,H).shift(3.5*DOWN+1*RIGHT)
           self.play(FadeIn(planedot))
           D=TexMobject(r"D").next_to(p0)
           for i in range (0,11):
             for j in range (0,11):
                 self.play(planedot[j][i].shift,(np.cos(1+i/3)*np.sin(1+j/3)+2)*Z_AXIS,run_time=.05)
           dx=TexMobject(r"dx").move_to(p[5][5].get_center()+(np.cos(1+5/3)*np.sin(1+5/2)+2.4)*Z_AXIS/3+(1/3+.1)*DOWN).rotate(PI/2,axis=X_AXIS).scale(.5)
           dx.rotate(PI/2,axis=Z_AXIS)
           dy=TexMobject(r"dy").move_to(p[5][5].get_center()+(np.cos(1+5/3)*np.sin(1+5/2)+2.5)*Z_AXIS/3+1/3*RIGHT).rotate(PI/2,axis=X_AXIS).scale(.5)
           self.wait(3)
           self.play(ShowCreation(plane,run_time=2))
           self.wait(1)
           self.play(ShowCreation(LX3D),ShowCreation(LY3D))
           self.play(FadeIn(HX),FadeIn(HY))
           self.play(ShowCreation(L3D))
           self.play(FadeIn(H))
           self.wait(3)
           self.play(FadeOut(planedot),FadeOut(H),FadeOut(L3D),FadeOut(HX),FadeOut(HY),FadeOut(LX3D),FadeOut(LY3D))
           self.stop_ambient_camera_rotation()
           self.play(FadeIn(p0))
           self.wait(1)
           self.play(FadeIn(D))
           self.play(ReplacementTransform(p0[5][5],p[5][5],run_time=3))
           dottorscotti1=Dot((0,0,0))
           dottorscotti2=Dot((np.cos(1+5/10)*np.sin(1+5/10)+2,0,0))
           k=VGroup(dottorscotti1,dottorscotti2)
           brace=Brace(mobject=k, direction=DOWN, buff=0).rotate(PI/2,axis=Y_AXIS).move_to(p[5][5].get_center()+0.13*LEFT+0.2*DOWN).scale(.7)
           text=brace.get_tex(r"f(x,y)").scale(.4).next_to(brace,LEFT).rotate(PI/2,axis=Y_AXIS)
           text.shift(-.2*Z_AXIS+0.2*DOWN)
           text.rotate(PI/2,axis=X_AXIS)
           self.play(GrowFromCenter(brace))
           self.play(FadeIn(text))
           self.play(FadeIn(dx),FadeIn(dy))
           self.wait(1)
           self.play(FadeOut(D),FadeOut(dx),FadeOut(dy),ReplacementTransform(p0,p,run_time=3),FadeOut(brace),FadeOut(text))
           self.wait(1)
           self.play(FadeOut(plane))
           self.play(ReplacementTransform(p,pp))
           self.play(ReplacementTransform(pp,ppp))
           self.play(ReplacementTransform(ppp,pppp))
           V=TexMobject(r"\int\int_D f(x,y)dxdy")
           V[0].shift(.23*RIGHT)
           self.add_fixed_in_frame_mobjects(V)
           V.to_corner(UL)
           self.play(FadeInFromDown(V))
           self.begin_ambient_camera_rotation(rate=0.01)
           self.wait(5)

class Dominio(ThreeDScene):
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
           axes = ThreeDAxes()
           grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
           grilla2=grilla.copy().next_to(grilla,UP,buff=0)
           grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
           grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
           grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
           grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
           grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
           self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
           funz=ParametricFunction(
                lambda u : np.array([
                u,
                3*np.exp(-(u+1)**2/3),
                0
            ]),color=RED,t_min=-6,t_max=2,
            )
           self.play(ShowCreation(funz))
           self.wait(1)
           rrrr=VGroup(*list(Rectangle(width=3/59, height=3*np.exp(-(-4+3*i/59+1)**2/3), color=BLUE, stroke_width=0) for i in range (0,110)))
           for i in range(0,110):
               rrrr[i].move_to((-4+3*i/59,3*np.exp(-(-4+3*i/59+1)**2/3)/2,0))
               rrrr[i].set_fill(BLUE, opacity=0.5) 
           rrrr.set_color_by_gradient(PURPLE,ORANGE)
           L=Line((-4+3*0/59,0,0),(-4+3*59/59,0,0)).set_color(PINK)
           LL=Line((-4+3*0/59,0,0),(-4+3*109/59,0,0)).set_color(PINK)
           inte=TexMobject(r"\int_a^bf(x)dx").to_corner(UL).shift(.2*LEFT)
           inte[1].set_color(GREEN)
           inte[2].set_color(PURPLE)
           a=TexMobject(r"a").move_to((-4,-0.2,0)).set_color(PURPLE)
           b=TexMobject(r"b").move_to((-1,-0.2,0)).set_color(GREEN)
           self.play(FadeIn(a),FadeIn(b),FadeInFromDown(inte),ShowCreation(rrrr[0:60]),ShowCreation(L))
           self.wait(1)
           self.play(ShowCreation(rrrr[60:110],run_time=2),ReplacementTransform(L,LL,run_time=2),b.shift,3*50/59*RIGHT,run_time=2)
           L1=Line((-4+3*20/59,0,0),(-4+3*59/59,0,0)).set_color(PINK)
           L2=Line((-4+3*80/59,0,0),(-4+3*109/59,0,0)).set_color(PINK)
           LINEE=VGroup(L1,L2)
           inte2=TexMobject(r"\int_a^cf(x)dx+\int_d^bf(x)dx").to_corner(UL).shift(.2*LEFT)
           inte2[1].set_color(RED)
           inte2[2].set_color(PURPLE)
           inte2[11].set_color(GREEN)
           inte2[12].set_color(BLUE)
           c=TexMobject(r"c").move_to((-1,-0.2,0)).set_color(RED)
           d=TexMobject(r"d").move_to((-4+3*80/59,-0.2,0)).set_color(BLUE)
           self.play(a.shift,+3*20/59*RIGHT,Transform(inte,inte2),FadeInFromLarge(c,2),FadeInFromLarge(d,2),Transform(rrrr[0:60],rrrr[20:60]),Transform(rrrr[60:110],rrrr[80:110]),ReplacementTransform(LL,LINEE))
           self.wait(1)
           self.play(FadeOut(inte),FadeOut(rrrr),FadeOut(funz),FadeOut(LINEE),FadeOut(a),FadeOut(b),FadeOut(c),FadeOut(d))
           r=Rectangle(width=4,height=2.5).set_fill(color=BLUE,opacity=.6)
           r.move_to((-3,2,0))
           r.set_color(YELLOW)
           c=Circle(radius=1.5).set_fill(color=BLUE,opacity=.6)
           c.move_to((2.5,2,0))
           c.set_color_by_gradient(PURPLE,ORANGE)
           curva1=ParametricFunction(
                lambda u : np.array([
                u,
                u**2/4,
                0
            ]),color=BLUE,t_min=-2.2,t_max=2.2,
            )
           curva2=ParametricFunction(
                lambda u : np.array([
                u,
                -u**2/2+3,
                0
            ]),color=BLUE,t_min=-2.2,t_max=2.2,
            )
           rbet=VGroup(*list(Rectangle(width=4/58, height=-(-2+4*i/59)**2/4-(-2+4*i/59)**2/2+3, color=BLUE, stroke_width=0) for i in range (0,60)))
           for i in range(0,60):
               rbet[i].move_to((-2+4*i/59,((-2+4*i/59)**2/4-(-2+4*i/59)**2/2+3)/2,0))
               rbet[i].set_fill(BLUE, opacity=0.5) 
           rrrr.set_color_by_gradient(PURPLE,ORANGE)
           g=VGroup(curva1,curva2,rbet)
           g.move_to(3*LEFT+2*DOWN)
           self.play(ShowCreation(r))
           self.wait(1)
           self.play(ShowCreation(curva1))
           self.play(ShowCreation(curva2))
           self.wait(1)
           self.play(ShowCreation(rbet))
           self.wait(1)
           e=Ellipse().move_to(2.5*RIGHT+2*DOWN).scale(2).set_color(PURPLE)
           e.set_fill(PURPLE,opacity=0.6)
           self.play(DrawBorderThenFill(c),DrawBorderThenFill(e))
           self.wait(1)
           self.play(FadeOut(g),FadeOut(c),FadeOut(e))
           self.play(r.move_to,c)
           #fai vedere prima gli estremi
           lineax1=DashedLine((2.5+2,0,0),(2.5+2,2+2.5/2,0))
           b=TexMobject(r"b").next_to(lineax1,DOWN).set_color(GREEN)
           lineax2=DashedLine((.5,0,0),(.5,2+2.5/2,0))
           a=TexMobject(r"a").next_to(lineax2,DOWN).set_color(PURPLE)
           lineay1=DashedLine((0,2-2.5/2,0),(2.5+2,2-2.5/2,0))
           c=TexMobject(r"c").next_to(lineay1,LEFT).set_color(RED)
           lineay2=DashedLine((0,2+2.5/2,0),(2.5+2,2+2.5/2,0))
           d=TexMobject(r"d").next_to(lineay2,LEFT).set_color(BLUE)
           self.play(ShowCreation(lineax1),ShowCreation(lineax2),ShowCreation(lineay2),ShowCreation(lineay1))
           planedot=VGroup(*list(VGroup(*list(Dot((.5+i/2,2-2.5/2+j/2,0)).scale(.5) for i in range (0,9))) for j in range (0,6))).set_color(YELLOW)
           self.play(FadeOut(r),ShowCreation(planedot))
           self.play(FadeInFromLarge(a,5))
           self.play(FadeInFromLarge(b,5))
           self.play(FadeInFromLarge(c,5))
           self.play(FadeInFromLarge(d,5))
           linx=DashedLine((.5+5/2,0,0),(.5+5/2,2-2.5/2+3/2,0)).set_color(YELLOW)
           liny=DashedLine((0,2-2.5/2+3/2,0),(.5+5/2,2-2.5/2+3/2,0)).set_color(YELLOW)
           xin=TexMobject(r"(x,y)\in [a,b]\times[c,d]").next_to(r,UP,buff=0.1)
           xin[7].set_color(GREEN)
           xin[9].set_color(PURPLE)
           xin[13].set_color(RED)
           xin[15].set_color(BLUE)
           self.wait(1)
           self.play(ShowCreation(linx),ShowCreation(liny))
           self.play(Write(xin))
           self.wait(1)
           self.play(FadeOut(planedot),FadeIn(r),Uncreate(linx),Uncreate(liny))
           R=Rectangle(height=TAU,width=4).move_to((2,PI,0)).set_color(YELLOW).set_fill(YELLOW,opacity=.6)
           R.shift(1*RIGHT)

           #SCENA 3D
           
           self.move_camera(phi=70*DEGREES,theta=20*DEGREES)
           self.begin_ambient_camera_rotation(rate=0.01)
           self.wait(1)
           Lettere=VGroup(a,b,c,d)
           linee=VGroup(lineax1,lineax2,lineay1,lineay2)
           self.play(FadeOut(xin),FadeOut(linx),FadeOut(liny),FadeOut(Lettere),FadeOut(linee))
           self.play(ReplacementTransform(r,R))
           self.wait(1)
           plane=ParametricSurface(
               lambda u,v : np.array([
                u,
                v,
               np.cos(u)*np.sin(v)+2
            ]),v_min=-1.1*TAU,v_max=1.1*TAU,u_min=-1.1*TAU,u_max=1.1*TAU,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7)
           self.play(ShowCreation(plane))
           def r(dx=1):##################CREATION OF THE AREA, WITH THE PARAMETER dx THAT IS MEANT TO SHIFT THE AREA ON THE X AXES###########################
            g= VGroup(*list(Rectangle(width=np.cos(dx)*np.sin(TAU*i/500)+2, height=1/20, color=BLUE, stroke_width=0) for i in range (0,500)))
            for i in range (0,500):
             g[i].set_shade_in_3d(True)
             g[i].set_fill(BLUE, opacity=0.5)
             g[i].move_to((dx,TAU*i/500,(np.cos(dx)*np.sin(TAU*i/500)+2)/2))
             g[i].rotate(PI/2,axis=Y_AXIS)
            g.set_submobject_colors_by_gradient(ORANGE, PURPLE)
            return g
           c=r()
           l1=Line((1,0,0),(1,2*PI,0)).set_color(YELLOW)
           l2=Line((1,2*PI,0),(5,2*PI,0)).set_color(YELLOW)
           l3=Line((5,2*PI,0),(5,0,0)).set_color(YELLOW)
           l4=Line((5,0,0),(1,0,0)).set_color(YELLOW)
           qu=VGroup(l1,l2,l3,l4)
           cl1=ParametricFunction(
                lambda u : np.array([
                1,
                u,
                np.cos(1)*np.sin(u)+2
            ]),color=RED,t_min=0,t_max=2*PI,
            )
           cl2=ParametricFunction(
                lambda u : np.array([
                u,
                2*PI,
                2
            ]),color=RED,t_min=1,t_max=5,
            )
           cl3=ParametricFunction(
                lambda u : np.array([
                5,
                u,
                np.cos(5)*np.sin(u)+2
            ]),color=RED,t_min=2*PI,t_max=0,
            )
           cl4=ParametricFunction(
                lambda u : np.array([
                u,
                0,
                2
            ]),color=RED,t_min=5,t_max=1,
            )    
           cqu=VGroup(cl1,cl2,cl3,cl4)
           hl1=DashedLine((1,0,0),(1,0,2)).set_color(YELLOW)
           hl2=DashedLine((1,2*PI,0),(1,2*PI,2)).set_color(YELLOW) 
           hl3=DashedLine((5,0,0),(5,0,2)).set_color(YELLOW) 
           hl4=DashedLine((5,2*PI,0),(5,2*PI,2)).set_color(YELLOW) 
           hqu=VGroup(hl1,hl2,hl3,hl4)
           self.wait()
           self.play(ShowCreation(hqu))
           self.wait()
           self.play(TransformFromCopy(qu,cqu))
           self.wait(1)
     ###### updating the area to show how the double integral works################
           def update_area(c, alpha):
            dx = interpolate(1, 5, alpha)
            c_c = r(dx)
            c.become(c_c) 
           def f(ddx=1):
             return ParametricFunction(
                 lambda u : np.array([
                  ddx,
                  u,
                 np.cos(ddx)*np.sin(u)+2
               ]),color=RED,t_min=0,t_max=TAU,
             )  
           k=f()     ###### updating the curve to follow the area################
           def update_f(k, alpha):
            ddx = interpolate(1, 5, alpha)
            k_k = f(ddx)
            k.become(k_k)
           self.play(FadeOut(plane)) 
           taglio=ParametricSurface(
               lambda u,v : np.array([
                1,
                v,
                u,
            ]),v_min=-1,v_max=3,u_min=-4,u_max=4,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7)
           taglio.rotate(PI/2,axis=X_AXIS).move_to(c.get_center()+0.43*LEFT+.3*Z_AXIS).set_fill(BLUE)
           plane.set_color(BLUE)
           self.play(ShowCreation(taglio))
           self.wait(1)
           inte=TexMobject(r"\int_c^d f(x_0,y)dy=F(x_0,d)-F(x_0,c)").move_to(c.get_center()+2*Z_AXIS).rotate(PI/2,axis=X_AXIS).scale(.6)
           inte[0:12].set_color_by_gradient(MAROON,BLUE)
           inte.rotate(PI/2,axis=Z_AXIS)
           self.play(Write(inte))
           self.wait(1)
           dinte=TexMobject(r"\int_a^b\left(\int_c^d f(x,y)dy\right)dx")
           dinte[1].set_color(GREEN)
           dinte[2].set_color(PURPLE)
           dinte[5].set_color(BLUE)
           dinte[6].set_color(RED)
           #= \\ \int_a^b\Big(F(x,d)-F(x,c)\Big)dx=\\ \mathcal{F}(b)-\mathcal{F}(a)
           dinte[0:3].shift(0.25*RIGHT)
           self.add_fixed_in_frame_mobjects(dinte)
           dinte.to_corner(UL)
           self.play(ShowCreation(c))
           self.wait(1)
           gg= VGroup(*list(Rectangle(width=np.cos(2.5)*np.sin(TAU*i/500)+2, height=1/20, color=BLUE, stroke_width=0) for i in range (0,500)))
           for i in range (0,500):
             gg[i].set_shade_in_3d(True)
             gg[i].set_fill(BLUE, opacity=0.5)
             gg[i].move_to((2.5,TAU*i/500,(np.cos(2.5)*np.sin(TAU*i/500)+2)/2))
             gg[i].rotate(PI/2,axis=Y_AXIS)
           gg.set_submobject_colors_by_gradient(GREEN, BLUE)
           cl11=ParametricFunction(
                lambda u : np.array([
                2.5,
                u,
                np.cos(2.5)*np.sin(u)+2
            ]),color=RED,t_min=0,t_max=2*PI,
            )
           ggg= VGroup(*list(Rectangle(width=np.cos(4.5)*np.sin(TAU*i/500)+2, height=1/20, color=BLUE, stroke_width=0) for i in range (0,500)))
           for i in range (0,500):
             ggg[i].set_shade_in_3d(True)
             ggg[i].set_fill(BLUE, opacity=0.5)
             ggg[i].move_to((4.5,TAU*i/500,(np.cos(4.5)*np.sin(TAU*i/500)+2)/2))
             ggg[i].rotate(PI/2,axis=Y_AXIS)
           ggg.set_submobject_colors_by_gradient(MAROON, PINK)
           cl111=ParametricFunction(
                lambda u : np.array([
                4.5,
                u,
                np.cos(4.5)*np.sin(u)+2
            ]),color=RED,t_min=0,t_max=2*PI,
            )
           x0=TexMobject(r"x=x_0").rotate(PI/2,axis=X_AXIS).scale(.8)
           x0.rotate(PI/2,axis=Z_AXIS)
           x0.move_to((1-0.43,-0.2,3))
           self.play(Write(x0),FadeOut(R))
           self.wait(1)
           piano=VGroup(x0,taglio,inte)
           self.play(piano.shift,1.5*RIGHT)
           self.play(ShowCreation(cl11),ShowCreation(gg))
           self.wait(1)
           self.play(piano.shift,2*RIGHT)
           self.play(ShowCreation(cl111),ShowCreation(ggg))
           self.wait(3)
           self.play(FadeOut(gg),FadeOut(ggg),piano.shift,3.5*LEFT,FadeOut(cl11),FadeOut(cl111))
           g1= VGroup(*list(Rectangle(width=2, height=1/30, color=BLUE, stroke_width=0) for i in range (0,500)))
           for i in range (0,500):
             g1[i].set_shade_in_3d(True)
             g1[i].set_fill(BLUE, opacity=0.5)
             g1[i].move_to((1+i/125,TAU,1))
             g1[i].rotate(PI/2,axis=Y_AXIS)
             g1[i].rotate(PI/2,axis=Z_AXIS)
           g1.set_submobject_colors_by_gradient(ORANGE, PURPLE)
           g2= VGroup(*list(Rectangle(width=2, height=1/30, color=BLUE, stroke_width=0) for i in range (0,500)))
           for i in range (0,500):
             g2[i].set_shade_in_3d(True)
             g2[i].set_fill(BLUE, opacity=0.5)
             g2[i].move_to((1+i/125,0,1))
             g2[i].rotate(PI/2,axis=Y_AXIS)
             g2[i].rotate(PI/2,axis=Z_AXIS)
           g2.set_submobject_colors_by_gradient(ORANGE, PURPLE)
           g3= VGroup(*list(ParametricFunction(lambda u : np.array([
                  1+i/62.5,
                  u,
                 np.cos(1+i/62.5)*np.sin(u)+2
               ]),color=RED,t_min=0,t_max=TAU,
             ) for i in range (0,250)))
           g3.set_submobject_colors_by_gradient(ORANGE, PURPLE)
           self.play(FadeOut(piano))
           #fare piano che passa con "quadro"?
           self.play(UpdateFromAlphaFunc(c,update_area),UpdateFromAlphaFunc(k,update_f),ShowCreation(g1,run_time=6),ShowCreation(g2,run_time=6),ShowCreation(g3,run_time=6),rate_func=linear,run_time=6)
           self.stop_ambient_camera_rotation()
          # text=TextMobject(r"\textbf{DOUBLE INTEGRALS}").scale(2).set_color_by_gradient(MAROON,ORANGE).shift(.4*UP)
          # st=TextMobject(r"\textbf{What are they for?}").set_color_by_gradient(MAROON,ORANGE)
          # logo=ImageMobject("logo").to_corner(DL).scale(.7).shift(.4*DOWN)
          # self.add_fixed_in_frame_mobjects(text,st,logo)
          # text.to_edge(UP)
          # st.next_to(text,DOWN)
          # self.add(text,st,logo)
           

           
class Esercizio(Scene):
    def construct(self):
      a=TexMobject(r"\int\int_D \cos (x+2y) dxdy").scale(2.9)
      a[0].shift(.9*RIGHT)
      D=TexMobject(r"D:=\Big\{(x,y)\in [0,\pi]\times\left[\frac{\pi}{2},\pi\right]\Big\}").to_edge(DOWN)
      D[11].set_color(PURPLE)
      D[13].set_color(GREEN)
      D[17:20].set_color(RED)
      D[21].set_color(BLUE)
      self.play(FadeInFromDown(a))
      self.wait(1)
      self.play(a.scale,.4)
      self.play(Write(D)) #DISEGNA IL RETTANGOLO
      self.wait(1)
      inte=TexMobject(r"\int_0^\pi\int_{\frac{\pi}{2}}^{\pi}").scale(1.2).move_to(a[0])
      inte[1].set_color(GREEN)
      inte[2].set_color(PURPLE)
      inte[4].set_color(BLUE)
      inte[5:8].set_color(RED)
      inte[0:3].shift(.4*RIGHT)
      self.play(Transform(a[0:3],inte))
      self.play(a.move_to,3*LEFT+2.5*UP)
      self.wait(1)
      a1=TexMobject(r"\int_0^\pi\left(\int_{\frac{\pi}{2}}^\pi \cos (x+2y) dy\right)dx").scale(1.2).next_to(a,DOWN).shift(.3*LEFT)
      a1[1].set_color(GREEN)
      a1[2].set_color(PURPLE)
      a1[5].set_color(BLUE)
      a1[6:9].set_color(RED)
      self.play(FadeInFromDown(a1))
      self.wait(1)
      a11=TexMobject(r"=\int_0^\pi\left[\frac{1}{2}\sin (x+2y)\right]_{\frac{\pi}{2}}^\pi dx").scale(1.2).next_to(a1,buff=.1)
      a11[2].set_color(GREEN)
      a11[3].set_color(PURPLE)
      a11[18].set_color(BLUE)
      a11[19:22].set_color(RED)
      a11[22:24].shift(.2*LEFT)
      b=Brace(mobject=a11[4:21],direction=UP).set_color(YELLOW)
      t=b.get_text(r"Area under the function \\ on the \textit{y} range \\ at a certain value of \textit{x}")
      t[0:4].set_color_by_gradient(ORANGE,PURPLE)
      t[20:31].set_color_by_gradient(BLUE,RED)
      t[31:49].set_color_by_gradient(GREEN,PURPLE)
      self.play(ShowCreation(a11[0]))
      self.play(Write(a11[1:24]))
      self.wait(1)
      self.play(GrowFromCenter(b))
      self.play(Write(t))
      a111=TexMobject(r"=\int_0^\pi\frac{1}{2}\left(\sin(x+2\pi)-\sin\left(x+2\frac{\pi}{2}\right)\right) dx").scale(1.2).next_to(a1[0],DOWN+RIGHT,buff=.4).shift(.3*LEFT+.1*DOWN)
      a111[2].set_color(GREEN)
      a111[3].set_color(PURPLE)
      a111[15].set_color(BLUE)
      a111[25:28].set_color(RED)
      self.play(ShowCreation(a111[0]))
      self.play(Write(a111[1:32]))
      self.wait(1)
      self.play(TransformFromCopy(a11[18],a111[15]),TransformFromCopy(a11[19:22],a111[25:28]))
      self.wait(1)
      sinx=TexMobject(r"\sin x").scale(1.2).next_to(a111[7])
      pimezzi=TexMobject(r"\cancel{2}\frac{\pi}{\cancel{2}}").scale(1.2).move_to(a111[24:28])
      pimezzi[0].set_color(RED)
      pimezzi[4].set_color(RED)
      #Transform(a111[8:17],sinx),
      self.play(ShowCreation(pimezzi[0]),ShowCreation(pimezzi[4]))
      sempl=VGroup(pimezzi[0],pimezzi[4],a111[24],a111[26:28])
      self.play(FadeOut(sempl),a111[25].next_to,a111[23])#self.play(a1[3:7].next_to,a1[11],LEFT,a1[7:11].next_to,a1[0:3],buff=0)
      self.play(a111[28:32].next_to,a111[25], buff=.01)
      self.wait(1)
      asempl=VGroup(a111[17:24],a111[25],a111[28:32])
      self.play(ReplacementTransform(a111[8:17],sinx),asempl.next_to, sinx,buff=0)
      asemplsempl=VGroup(a111[18:24],a111[25],a111[28])
      self.wait(1)#di che è in virtu degli angoli associati o fai vedere
      menosinx=TexMobject(r"-\sin x").scale(1.2).next_to(a111[17])
      piu=TexMobject(r"+").scale(1.2).move_to(a111[17])
      self.play(ReplacementTransform(asemplsempl,menosinx),a111[29:32].next_to,menosinx,buff=0)
      k=VGroup(a111[17],menosinx[0])
      kk=VGroup(menosinx[1:5],a111[29:32])
      self.play(ReplacementTransform(k,piu),kk.next_to,piu)
      duesinx=TexMobject(r"2\sin x").scale(1.2).next_to(a111[5])
      SEMPL=VGroup(menosinx,sinx)
      self.wait(1)
      self.play(FadeOut(a111[7]),FadeOut(a111[29]),ReplacementTransform(SEMPL,duesinx),FadeOut(piu),a111[30:32].next_to,duesinx)
      self.wait(1)
      ultima=VGroup(a111[4:7],duesinx[0])
      canc=TexMobject(r"\frac{1}{\cancel{2}}\cancel{2}").scale(1.2).move_to(ultima)
      canc[2].set_color(RED)
      canc[4].set_color(RED).move_to(duesinx[0])
      self.play(ShowCreation(canc[2]),ShowCreation(canc[4]))
      self.wait(1)
      self.play(FadeOut(a111[4:7]),FadeOut(duesinx[0]),FadeOut(canc[2]),FadeOut(canc[4]))
      nevero=VGroup(duesinx[1:5],a111[30:32])
      self.play(nevero.next_to,a111[1])
      self.wait(1)
      gruppone=VGroup(a111[1:4],nevero,a111[30:32])
      b2=Brace(mobject=gruppone,direction=DOWN).set_color(YELLOW)
      t2=b2.get_text(r"Sum of all the areas \\ for all the values of $x$ \\ in its range").scale(.7).shift(.4*UP)
      t2[11:16].set_color_by_gradient(ORANGE,PURPLE)
      t2[16:44].set_color_by_gradient(GREEN,PURPLE)
      self.play(GrowFromCenter(b2),FadeOut(D))
      self.play(Write(t2))
      self.wait(1)
      soluzio=TexMobject(r"=-\cos(\pi)-(-\cos(0))=2").scale(1.2).next_to(a111[31])
      soluzio[6].set_color(GREEN)
      soluzio[15].set_color(PURPLE)
      uno=TexMobject(r"1").scale(1.2).next_to(soluzio[0])
      self.play(ShowCreation(soluzio[0]))
      self.play(Write(soluzio[1:18]))
      self.wait(1)
      self.play(TransformFromCopy(a111[2],soluzio[6]),TransformFromCopy(a111[3],soluzio[15]))
      self.wait(1)
      self.play(Transform(soluzio[1:8],uno),soluzio[8:18].next_to,uno)
      uuno=uno.copy().next_to(soluzio[10])
      self.play(Transform(soluzio[11:17],uuno),soluzio[17].next_to,uuno)
      soluzio[18:20].next_to(soluzio[17])
      self.play(ShowCreation(soluzio[18]))
      self.play(Write(soluzio[19]))
      r=SurroundingRectangle(soluzio[19])
      self.play(ShowCreationThenDestruction(r))
      self.wait(1)
      togli=VGroup(a11,a111[0:4],soluzio,b,t,b2,t2,duesinx[1:5],a111[30:32])
      self.play(FadeOut(togli))
      self.wait(1)

      self.play(Swap(a1[0:3],a1[4:9]),Swap(a1[18:20],a1[21:23]))
      self.play(a1[18:20].shift,.1*DOWN)
      self.wait(1)
      a22=TexMobject(r"=\int_{\frac{\pi}{2}}^\pi\left[\sin (x+2y)\right]_{0}^\pi dy").scale(1.2).next_to(a1,buff=.1)
      a22[2].set_color(BLUE)
      a22[3:6].set_color(RED)
      a22[17].set_color(GREEN)
      a22[18].set_color(PURPLE)
      a22[22:24].shift(.2*LEFT)
      self.play(ShowCreation(a22[0]))
      self.play(Write(a22[1:24]))
      self.wait(1)
      a222=TexMobject(r"=\int_{\frac{\pi}{2}}^\pi\left(\sin(\pi+2y)-\sin(0+2y)\right) dy").scale(1.2).next_to(a1[4],DOWN+RIGHT,buff=.4).shift(.3*LEFT+.1*DOWN)
      a222[2].set_color(BLUE)
      a222[3:6].set_color(RED)
      a222[11].set_color(GREEN)
      a222[21].set_color(PURPLE)
      self.play(ShowCreation(a222[0]))
      self.play(Write(a222[1:32]))
      self.wait(1)
      self.play(TransformFromCopy(a22[17],a222[21]),TransformFromCopy(a22[18],a222[11]))
      self.wait(1)
      menosiny=TexMobject(r"-\sin(2y)").scale(1.2).next_to(a222[6])
      self.play(ReplacementTransform(a222[7:16],menosiny))
      self.play(FadeOut(a222[21:23]),a222[23:29].next_to,a222[20],buff=0)
      self.wait(1)
      menoduesiny=TexMobject(r"-2\sin(2y)").scale(1.2).next_to(a222[1])
      k2=VGroup(menosiny,a222[16:21],a222[23:26])
      self.play(FadeOut(a222[6]),FadeOut(a222[26]),ReplacementTransform(k2,menoduesiny),a222[27:29].next_to,menoduesiny)
      self.wait(1)
      self.play(menoduesiny[0:2].next_to,a222[0],a222[1:6].next_to,menoduesiny[2],LEFT,menoduesiny[2:9].shift,.4*LEFT,a222[27:29].shift,.4*LEFT)
      ris=TexMobject(r"=-\cancel{2}\frac{1}{\cancel{2}}\left(-\cos(2\pi)--\cos\left(\cancel{2}\frac{\pi}{\cancel{2}}\right)\right)").scale(1.2).next_to(a222[28])
      ris[15].set_color(BLUE)
      ris[25:27].set_color(RED)
      ris[28].set_color(RED)
      cancel=VGroup(ris[2],ris[6],ris[23],ris[27]).set_color(RED)
      nocancel=VGroup(ris[1],ris[3:6],ris[7:23],ris[24:27],ris[28:32])
      self.wait(1)
      self.play(ShowCreation(ris[0]))
      self.play(Write(nocancel))
      self.wait(1)
      self.play(ShowCreation(cancel))
      self.play(FadeOut(ris[2:8]),FadeOut(ris[23:25]),FadeOut(ris[26:29]),ris[1].next_to,ris[8],LEFT,ris[25].next_to,ris[22])
      self.wait(1)
      luno=TexMobject(r"1").scale(1.2).next_to(ris[9])
      luuno=TexMobject(r"1").scale(1.2).next_to(ris[17])
      bastah=VGroup(ris[18:23],ris[25],ris[29])
      self.play(Transform(ris[10:17],luno),ReplacementTransform(bastah,luuno),ris[30:32].next_to,luuno)
      nooo=VGroup(ris[17],luuno,ris[30:32])
      self.play(nooo.next_to,luno)
      soluzio2=TexMobject(r"=2").scale(1.2).next_to(ris[30])
      self.play(ShowCreation(soluzio2[0]))
      self.play(Write(soluzio2[1]))
      r2=SurroundingRectangle(soluzio2[1])
      self.play(ShowCreationThenDestruction(r2))
      works=TextMobject(r"IT WORKS!").scale(2).to_edge(DOWN).set_color_by_gradient(RED,ORANGE)
      self.play(Write(works))
      self.wait(1)

class Esercizio2(ThreeDScene):
    CONFIG = {
        "y_max" : 1.5,
        "y_min" : 0,
        "x_max" : 1.5,
        "x_min" : 0,
        "y_tick_frequency" : 1, 
        "x_tick_frequency" : 1, 
    }
    def construct(self):
      a=TexMobject(r"\int\int_D xy\, dxdy").scale(2.9)
      a[0].shift(.9*RIGHT)
      D=TexMobject(r"D:=\Big\{(x,y)\in [0,1]\times\left[x^2,x\right]\Big\}").to_edge(DOWN).shift(1*LEFT)
      D[11].set_color(PURPLE)
      D[13].set_color(GREEN)
      D[17:19].set_color(BLUE)
      D[20].set_color(BLUE)
      self.play(FadeInFromDown(a))
      self.wait(1)
      self.play(a.scale,.4)
      self.play(Write(D)) #DISEGNA IL RETTANGOLO
      self.wait(1)
      inte=TexMobject(r"\int_0^1\int_{x^2}^{x}").scale(1.2).move_to(a[0])
      inte[1].set_color(GREEN)
      inte[2].set_color(PURPLE)
      inte[4].set_color(BLUE)
      inte[5:8].set_color(BLUE)
      inte[0:3].shift(.4*RIGHT)
      self.play(Transform(a[0:3],inte))
      self.wait(1)
      
      axes = ThreeDAxes()

      curva1=ParametricFunction(
                lambda u : np.array([
                u,
                u**2/4,
                0
            ]),color=BLUE,t_min=0,t_max=4,
            )
      curva2=ParametricFunction(
                lambda u : np.array([
                u,
                u,
                0
            ]),color=BLUE,t_min=0,t_max=4,
            )
      rbet=VGroup(*list(Rectangle(width=4/59, height=(4*i/59)**2/4-4*(i/59), color=BLUE, stroke_width=0) for i in range (0,60)))
      for i in range(0,60):
               rbet[i].move_to((4*i/59,((4*i/59)**2/4+4*(i/59))/2,0))
               rbet[i].set_fill(BLUE, opacity=0.5) 
      d=DashedLine((2,0,0),(2, 2,0)).set_color(YELLOW)
      d2=DashedLine((0,2,0),(2, 2,0)).set_color(YELLOW)
      d3=DashedLine((0,1,0),(2, 1,0)).set_color(YELLOW)
      X=TexMobject(r"x^2").set_color(BLUE).move_to(curva1.get_center()+1.5*RIGHT)
      X2=TexMobject(r"x").set_color(BLUE).next_to(curva2.get_center()+1*UP)
      xX=TexMobject(r"x").next_to(d,DOWN)
      cX=TexMobject(r"c(x)").next_to(d3,LEFT)
      cX[0].set_color(RED)
      dX=TexMobject(r"d(x)").next_to(d2,LEFT)
      dX[0].set_color(BLUE)
      br=Brace(mobject=rbet[29],direction=RIGHT).set_color(YELLOW)
      tr=br.get_text(r"y range").set_color_by_gradient(BLUE,RED)
      group = VGroup(curva1,curva2,axes,rbet,d,d2,d3,X,X2,cX,dX,xX,br,tr)
      group.scale_in_place(.6)
      group.move_to(4.5*RIGHT+1.8*DOWN)
      self.play(ShowCreation(axes))
      self.play(ShowCreation(curva1),FadeIn(X))
      self.play(ShowCreation(curva2),FadeIn(X2))
      self.play(ShowCreation(rbet))
      self.wait(1)
      self.play(a.move_to,3*LEFT+2.5*UP)
      a1=TexMobject(r"\int_0^1\left(\int_{x^2}^x xy dy\right)dx").scale(1.2).next_to(a,DOWN).shift(.3*LEFT)
      a1[1].set_color(GREEN)
      a1[2].set_color(PURPLE)
      a1[5].set_color(BLUE)
      a1[6:8].set_color(BLUE)
      self.play(FadeInFromDown(a1))
      self.wait(1)
      self.play(a1[3:8].shift,.5*RIGHT,a1[8].shift,1.3*LEFT)
      self.wait(1)
      a11=TexMobject(r"=\int_0^1x\left[\frac{y^2}{2}\right]_{x^2}^x dx").scale(1.2).next_to(a1,buff=.1)
      a11[2].set_color(GREEN)
      a11[3].set_color(PURPLE)
      a11[11:14].set_color(BLUE)
      b=Brace(mobject=a11[4:14],direction=UP).set_color(YELLOW)
      t=b.get_text(r"Area under the function \\ on the \textit{y} range \\ at a certain value of \textit{x}").scale(.8) #is still this, but the area will depend on which x i am not only for the position, but also for the range on the y-axis
      t[0:4].set_color_by_gradient(ORANGE,PURPLE)
      t[20:31].set_color_by_gradient(BLUE,RED)
      t[31:49].set_color_by_gradient(GREEN,PURPLE)
      self.play(ShowCreation(a11[0]))
      self.play(Write(a11[1:18]))
      self.wait(1)
      self.play(GrowFromCenter(b))
      self.play(Write(t))
      self.play(ShowCreation(d),FadeIn(xX))
      self.play(ShowCreation(d2),ShowCreation(d3),FadeIn(cX),FadeIn(dX))
      self.play(GrowFromCenter(br))
      self.play(Write(tr))
      self.wait(2)
      a111=TexMobject(r"=\int_0^1x\left(\frac{(x)^2}{2}-\frac{(x^2)^2}{2}\right) dx").scale(1.2).next_to(a1[0],DOWN+.1*RIGHT,buff=.4).shift(.3*LEFT+.1*DOWN)
      a111[2].set_color(GREEN)
      a111[3].set_color(PURPLE)
      a111[7].set_color(BLUE)
      a111[14:16].set_color(BLUE)
      self.play(ShowCreation(a111[0]))
      self.play(Write(a111[1:23]))
      self.wait(1)
      allaseconda=TexMobject(r"x^2").scale(1.2).move_to(a111[6:10])
      allaquarta=TexMobject(r"x^4").scale(1.2).move_to(a111[13:18])
      self.play(TransformFromCopy(a11[11],a111[7]),TransformFromCopy(a11[12:14],a111[14:16]))
      self.wait(1)
      self.play(Transform(a111[6:10],allaseconda),Transform(a111[13:18],allaquarta))
      self.wait(1)
      x=a111[4].copy()
      self.add(x)
      allaterza=TexMobject(r"x^3").scale(1.2).move_to(a111[6:10])  
      allaquinta=TexMobject(r"x^5").scale(1.2).move_to(a111[13:18])  
      self.play(Transform(a111[4],allaterza),Transform(a111[6:10],allaterza),Transform(x,allaquinta),Transform(a111[13:18],allaquinta))
      self.play(a111[4:23].shift,.7*LEFT,x.shift,.7*LEFT)
      sol=TexMobject(r"=\frac{1^4}{8}-\frac{0^4}{8}-\frac{1^6}{12}+\frac{0^6}{12}").scale(1.2).next_to(a111[22],buff=SMALL_BUFF)
      sol[1].set_color(GREEN)
      sol[6].set_color(PURPLE)
      sol[11].set_color(GREEN)
      sol[17].set_color(PURPLE)
      b2=Brace(mobject=a111[1:23],direction=DOWN).set_color(YELLOW)
      t2=b2.get_text(r"Sum of all the areas \\ for all the values of $x$ \\ in its range").scale(.7).shift(.4*UP)
      t2[11:16].set_color_by_gradient(ORANGE,PURPLE)
      t2[16:44].set_color_by_gradient(GREEN,PURPLE)
      self.play(GrowFromCenter(b2),FadeOut(D))
      self.play(Write(t2))
      self.wait(1)
      for i in range(0,60):
        rbet[i].save_state()
      for i in range(0,60):
        self.play(rbet[i].set_color,YELLOW,run_time=0.004)
        self.play(Restore(rbet[i],run_time=0.004))
      self.wait(1)
      self.play(ShowCreation(sol[0]),FadeOut(group))
      self.play(Write(sol[1:22]))
      feidda=VGroup(sol[2],sol[5:10],sol[12],sol[16:22])
      un=VGroup(sol[10:12],sol[13:16])
      self.play(FadeOut(feidda),un.next_to,sol[1:5])
      self.play(sol[1].shift,.1*RIGHT,sol[11].shift,.1*RIGHT)
      venti=TexMobject(r"=\frac{1}{24}").scale(1.2).next_to(un)
      self.play(ShowCreation(venti[0]))
      self.play(Write(venti[1:5]))
      r=SurroundingRectangle(venti[1:5])
      self.play(ShowCreationThenDestruction(r))
      self.wait(1)

class DominioNormale(ThreeDScene): #ricomputa
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
      a=TexMobject(r"\int_a^b\int_{g_1(x)}^{g_2(x)} f(x,y) dxdy").scale(2)
      a[0:3].shift(.3*RIGHT)
      a[1].set_color(GREEN)
      a[2].set_color(PURPLE)
      a[9:14].set_color(BLUE)
      a[4:9].set_color(BLUE)
      a[5].set_color(YELLOW)
      a[10].set_color(YELLOW)
      self.play(FadeInFromDown(a))
      self.wait(1)
      aa=TexMobject(r"\int_a^b\int_{g_1(x)}^{g_2(x)} f(x,y) dxdy").to_corner(UL)
      aa[1].set_color(GREEN)
      aa[2].set_color(PURPLE)
      aa[9:14].set_color(BLUE)
      aa[4:9].set_color(BLUE)
      aa[5].set_color((YELLOW))
      aa[10].set_color(YELLOW)
      aa[0:3].shift(.15*RIGHT)
      axes = ThreeDAxes()
      grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
      grilla2=grilla.copy().next_to(grilla,UP,buff=0)
      grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
      grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
      grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
      grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
      grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
      self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
      self.play(Transform(a,aa),ShowCreation(axes),ShowCreation(grilla))
      self.wait(1)

      curva1=ParametricFunction(
                lambda u : np.array([
                u,
                u**2/4,
                0
            ]),color=BLUE,t_min=-2.2,t_max=2.2,
            )
      funz1=TexMobject(r"g_1(x)").scale(.7).next_to(curva1).shift(2.7*RIGHT).set_color(BLUE)
      funz1[1].set_color(YELLOW)
      curva2=ParametricFunction(
                lambda u : np.array([
                u,
                -u**2/2+3,
                0
            ]),color=BLUE,t_min=-2.2,t_max=2.2,
            )
      funz2=TexMobject(r"g_2(x)").scale(.7).next_to(curva2).shift(2.2*RIGHT+1.4*UP).set_color(BLUE)      
      funz2[1].set_color(YELLOW)
      rbet=VGroup(*list(Rectangle(width=4/58, height=-(-2+4*i/59)**2/4-(-2+4*i/59)**2/2+3, color=BLUE, stroke_width=0) for i in range (0,60)))
      for i in range(0,60):
               rbet[i].move_to((-2+4*i/59,((-2+4*i/59)**2/4-(-2+4*i/59)**2/2+3)/2,0))
               rbet[i].set_fill(BLUE, opacity=0.5) 
      g=VGroup(curva1,curva2,rbet)
      g.shift(3*RIGHT+ .5*UP)
      self.play(ShowCreation(curva1),FadeIn(funz1))
      self.play(ShowCreation(curva2),FadeIn(funz2))
      self.play(ShowCreation(rbet))
      self.play(TransformFromCopy(funz1[0],a[9]),TransformFromCopy(funz1[1],a[10]),TransformFromCopy(funz1[2],a[11]),TransformFromCopy(funz1[3],a[12]),TransformFromCopy(funz1[4],a[13]),TransformFromCopy(funz2[0],a[4]),TransformFromCopy(funz2[1],a[5]),TransformFromCopy(funz2[2],a[6]),TransformFromCopy(funz2[3],a[7]),TransformFromCopy(funz2[4],a[8]))
      ax=DashedLine((1,0,0),(1,1.5,0)).set_color(YELLOW)
      bx=DashedLine((5,0,0),(5,1.5,0)).set_color(YELLOW)
      self.wait(1)
      self.play(ShowCreation(ax),ShowCreation(bx))
      A=TexMobject(r"a").set_color(PURPLE).next_to(ax,DOWN)
      B=TexMobject(r"b").set_color(GREEN).next_to(bx,DOWN)
      self.play(FadeInFromLarge(A,5))
      self.play(FadeInFromLarge(B,5))
      def lineax(dx=-2):
         return DashedLine((dx,-.5,0),(dx, -(dx)**2/2+3,0)).set_color(YELLOW).shift(3*RIGHT+ .5*UP)
      lx=lineax()
      def update_lineax(lx, alpha):
          dx=interpolate(-2,2,alpha)
          lxnuovo=lineax(dx)
          lx.become(lxnuovo)
      def lineay1(dy=1):
               return DashedLine((0,-(dy-3)**2/2+3.5,0),(dy,-(dy-3)**2/2+3.5,0)).set_color(YELLOW)
      ly=lineay1()
      def update_lineay(ly, beta):
                dy=interpolate(1,5,beta)
                lynuovo=lineay1(dy)
                ly.become(lynuovo)
      def lineay2(dy2=1):
               return DashedLine((0,(dy2-3)**2/4+.5,0),(dy2,(dy2-3)**2/4+.5,0)).set_color(YELLOW)
      ly2=lineay2()
      def update_lineay2(ly2, beta):
                dy2=interpolate(1,5,beta)
                lynuovo2=lineay2(dy2)
                ly2.become(lynuovo2)
      C=TexMobject(r"d(x)")
      C[0].set_color(BLUE)
      C.add_updater(lambda m: m.next_to(ly,LEFT))
      D=TexMobject(r"c(x)")
      D[0].set_color(RED)
      D.add_updater(lambda m: m.next_to(ly2,LEFT))
      X=TexMobject(r"x")
      X.add_updater(lambda m: m.next_to(lx,DOWN))
      self.play(FadeIn(C),FadeIn(D),FadeIn(X),ShowCreation(ly),ShowCreation(ly2))
      self.play(UpdateFromAlphaFunc(lx,update_lineax),UpdateFromAlphaFunc(ly,update_lineay),UpdateFromAlphaFunc(ly2,update_lineay2), run_time=8)
      gruppone=VGroup(A,B,C,D,ax,bx,lx,ly,ly2,X,funz1,funz2,a)
      self.play(FadeOut(gruppone))
      self.wait(1)

      #SCENA 3D

      self.move_camera(phi=70*DEGREES,theta=20*DEGREES)
      self.begin_ambient_camera_rotation(rate=0.01)
      self.wait(1)
      plane=ParametricSurface(
               lambda u,v : np.array([
                u,
                v,
               np.cos(1.4*u+1)*np.sqrt(v+1)/2+3
            ]),v_min=-2.2,v_max=4.2,u_min=-2.2,u_max=2.2,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7)     
      plane.shift(3*RIGHT+ .5*UP)
      self.play(ShowCreation(plane))
      self.wait(1)

      rect=VGroup(*list(Rectangle(width=1/30, height=np.cos(1.4*0+1)*np.sqrt(3*i/500+1)/2+3, color=BLUE, stroke_width=0) for i in range (0,500)))
      for i in range (0,500):
            rect[i].rotate(PI/2,axis=X_AXIS)
            rect[i].rotate(PI/2,axis=Z_AXIS)
            rect[i].set_shade_in_3d(True)
            rect[i].set_fill(BLUE, opacity=0.5)
            rect[i].move_to((0,3*i/500,(np.cos(1.4*0+1)*np.sqrt(3*i/500+1)/2+3)/2))
      rect.shift(3*RIGHT+ .5*UP) 
      rect.set_submobject_colors_by_gradient(GREEN, BLUE) 
      rect0=VGroup(*list(Rectangle(width=1/30, height=.01, color=BLUE, stroke_width=0) for i in range (0,500)))
      for i in range (0,500):
            rect0[i].rotate(PI/2,axis=X_AXIS)
            rect0[i].rotate(PI/2,axis=Z_AXIS)
            rect0[i].set_shade_in_3d(True)
            rect0[i].set_fill(BLUE, opacity=0.5)
            rect0[i].move_to((0,3*i/500,.01/2))
      rect0.shift(3*RIGHT+ .5*UP)   
      rect0.set_submobject_colors_by_gradient(GREEN, BLUE)
      self.play(ReplacementTransform(rect0,rect,run_time=3))
      figo=ParametricFunction(
                lambda u : np.array([
                0,
                u,
                np.cos(1.4*0+1)*np.sqrt(u+1)/2+3
            ]),color=RED,t_min=0,t_max=3,
            ).shift(3*RIGHT+ .5*UP)
      self.play(ShowCreationThenDestruction(figo))
      self.wait(1)
      k1=DashedLine((0,-(3-3)**2/2+3.5,0),(3,-(3-3)**2/2+3.5,0)).set_color(YELLOW)
      k2=DashedLine((0,(3-3)**2/4+.5,0),(3,(3-3)**2/4+.5,0)).set_color(YELLOW)
      C=TexMobject(r"d(x)").shift(.2*Z_AXIS).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS)
      C[0].set_color(BLUE)
      C.next_to(k1,LEFT+.1*Z_AXIS)
      D=TexMobject(r"c(x)").shift(.2*Z_AXIS).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS)
      D[0].set_color(RED)
      D.next_to(k2,LEFT+.1*Z_AXIS)
      self.play(FadeIn(k1),FadeIn(k2),FadeIn(C),FadeIn(D))
      rect2=VGroup(*list(Rectangle(width=1/30, height=np.cos(1.4*.7+1)*np.sqrt((.7)**2/4+(-(.7)**2/2+3-(.7)**2/4)*i/500+1)/2+3, color=BLUE, stroke_width=0) for i in range (0,500)))
      for i in range (0,500):
            rect2[i].rotate(PI/2,axis=X_AXIS)
            rect2[i].rotate(PI/2,axis=Z_AXIS)
            rect2[i].set_shade_in_3d(True)
            rect2[i].set_fill(BLUE, opacity=0.5)
            rect2[i].move_to((.7,(.7)**2/4+(-(.7)**2/2+3-(.7)**2/4)*i/500,(np.cos(1.4*.7+1)*np.sqrt((.7)**2/4+(-(.7)**2/2+3-(.7)**2/4)*i/500+1)/2+3)/2))
      rect2.shift(3*RIGHT+ .5*UP) 
      rect2.set_submobject_colors_by_gradient(GREEN, BLUE)

      rect3=VGroup(*list(Rectangle(width=1/30, height=np.cos(1.4*1.4+1)*np.sqrt((1.4)**2/4+(-(1.4)**2/2+3-(1.4)**2/4)*i/500+1)/2+3, color=BLUE, stroke_width=0) for i in range (0,500)))
      for i in range (0,500):
            rect3[i].rotate(PI/2,axis=X_AXIS)
            rect3[i].rotate(PI/2,axis=Z_AXIS)
            rect3[i].set_shade_in_3d(True)
            rect3[i].set_fill(BLUE, opacity=0.5)
            rect3[i].move_to((1.4,(1.4)**2/4+(-(1.4)**2/2+3-(1.4)**2/4)*i/500,(np.cos(1.4*1.4+1)*np.sqrt((1.4)**2/4+(-(1.4)**2/2+3-(1.4)**2/4)*i/500+1)/2+3)/2))
      rect3.shift(3*RIGHT+ .5*UP) 
      rect3.set_submobject_colors_by_gradient(GREEN, BLUE)

      k3=DashedLine((0,-(3.7-3)**2/2+3.5,0),(3.7,-(3.7-3)**2/2+3.5,0)).set_color(YELLOW)
      k4=DashedLine((0,(3.7-3)**2/4+.5,0),(3.7,(3.7-3)**2/4+.5,0)).set_color(YELLOW)

      self.play(FadeOut(plane),ReplacementTransform(rect,rect2),ReplacementTransform(k1,k3),ReplacementTransform(k2,k4),C.next_to,k3,LEFT+.1*Z_AXIS,D.next_to,k4,LEFT+.1*Z_AXIS)
      self.wait(1)

      k5=DashedLine((0,-(4.4-3)**2/2+3.5,0),(4.4,-(4.4-3)**2/2+3.5,0)).set_color(YELLOW)
      k6=DashedLine((0,(4.4-3)**2/4+.5,0),(4.4,(4.4-3)**2/4+.5,0)).set_color(YELLOW)

      self.play(ReplacementTransform(rect2,rect3),ReplacementTransform(k3,k5),ReplacementTransform(k4,k6),C.next_to,k5,LEFT+.1*Z_AXIS,D.next_to,k6,LEFT+.1*Z_AXIS)
      self.wait(1)
      
      rect4=VGroup(*list(Rectangle(width=1/30, height=np.cos(1.4*1.81+1)*np.sqrt((1.81)**2/4+(-(1.81)**2/2+3-(1.81)**2/4)*i/500+1)/2+3, color=BLUE, stroke_width=0) for i in range (0,500)))
      for i in range (0,500):
            rect4[i].rotate(PI/2,axis=X_AXIS)
            rect4[i].rotate(PI/2,axis=Z_AXIS)
            rect4[i].set_shade_in_3d(True)
            rect4[i].set_fill(BLUE, opacity=0.5)
            rect4[i].move_to((1.81,(1.81)**2/4+(-(1.81)**2/2+3-(1.81)**2/4)*i/500,(np.cos(1.4*1.81+1)*np.sqrt((1.81)**2/4+(-(1.81)**2/2+3-(1.81)**2/4)*i/500+1)/2+3)/2))
      rect4.shift(3*RIGHT+ .5*UP) 
      rect4.set_submobject_colors_by_gradient(GREEN, BLUE)

      k7=DashedLine((0,-(4.81-3)**2/2+3.5,0),(4.81,-(4.81-3)**2/2+3.5,0)).set_color(YELLOW)
      k8=DashedLine((0,(4.81-3)**2/4+.5,0),(4.81,(4.81-3)**2/4+.5,0)).set_color(YELLOW)

      self.play(ReplacementTransform(rect3,rect4),ReplacementTransform(k5,k7),ReplacementTransform(k6,k8),C.next_to,k7,LEFT+.1*Z_AXIS,D.next_to,k8,LEFT+.1*Z_AXIS)
      self.wait(1)

      dinte=TexMobject(r"\int_a^b\left(\int_{c(x)}^{d(x)} f(x,y)dy\right)dx")
      dinte[1].set_color(GREEN)
      dinte[2].set_color(PURPLE)
      dinte[5].set_color(BLUE)
      dinte[9].set_color(RED)
      dinte[0:3].shift(0.25*RIGHT)
      self.add_fixed_in_frame_mobjects(dinte)
      dinte.to_corner(UL)
      self.play(FadeInFromDown(dinte)) 
      self.wait(1)     
      naltrogruppone=VGroup(k7,k8,rect4,C,D)
      self.play(FadeOut(naltrogruppone))
      g1= VGroup(*list(ParametricFunction(lambda u : np.array([
                  -2+i/62.5,
                  (-2+i/62.5)**2/4,
                 u,
               ]),color=RED,t_min=0,t_max=np.cos(1.4*(-2+i/62.5)+1)*np.sqrt((-2+i/62.5)**2/4+1)/2+3,
             ) for i in range (0,250)))
      g1.set_submobject_colors_by_gradient(GREEN,BLUE)
      g1.shift(3*RIGHT+ .5*UP)
      g2= VGroup(*list(ParametricFunction(lambda u : np.array([
                  -2+i/75,
                  -(-2+i/75)**2/2+3,
                 u,
               ]),color=RED,t_min=0,t_max=np.cos(1.4*(-2+i/75)+1)*np.sqrt(-(-2+i/75)**2/2+3+1)/2+3,
             ) for i in range (0,300)))
      g2.set_submobject_colors_by_gradient(GREEN,BLUE)
      g2.shift(3*RIGHT+ .5*UP)
      g3= VGroup(*list(ParametricFunction(lambda u : np.array([
                  -2+i/62.5,
                  u,
                 np.cos(1.4*(-2+i/62.5)+1)*np.sqrt(u+1)/2+3
               ]),color=RED,t_min=(-2+i/62.5)**2/4,t_max=-(-2+i/62.5)**2/2+3,
             ) for i in range (0,250)))
      g3.set_submobject_colors_by_gradient(BLUE,GREEN)
      g3.shift(3*RIGHT+ .5*UP)
      def l(dx=-2):##################CREATION OF THE AREA, WITH THE PARAMETER dx THAT IS MEANT TO SHIFT THE AREA ON THE X AXES###########################
            linee= VGroup(*list(Line((dx,dx**2/4,(np.cos(1.4*(dx)+1)*np.sqrt(dx**2/4+1)/2+3)*i/500),(dx,-(dx)**2/2+3,(np.cos(1.4*(dx)+1)*np.sqrt((-(dx)**2/2+3)**2/4+1)/2+3)*i/500)) for i in range (0,500)))
            linee.set_submobject_colors_by_gradient(BLUE,GREEN)
            linee.shift(3*RIGHT+ .5*UP)
            return linee
      c=l()
      def update_area(c, alpha):
            dx = interpolate(-2, 1.99, alpha)
            c_c = l(dx)
            c.become(c_c) 
      self.play(ShowCreation(g1,run_time=6),ShowCreation(g2,run_time=6),ShowCreation(g3,run_time=6),UpdateFromAlphaFunc(c,update_area), run_time=6)
      self.wait(1)
      plane2=ParametricSurface(
               lambda u,v : np.array([
                u,
                v,
               np.cos(1.4*u+1)*np.sqrt(v+1)/2+3
            ]),v_min=-0.5,v_max=4.2,u_min=-2.2,u_max=1.9,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).fade(.7)     
      plane2.shift(3*RIGHT+ .5*UP)
      self.play(ShowCreation(plane2))
      self.play(Uncreate(plane2))
      self.wait(3)
#FAI CLASS "MAGRA CONSOLAZIONE": MICA POSSO FARE LETTERALMENTE LA SOMMA DI INFINITI VOLUMI, MA POSSO RICONDURMI CON TRUCCHETTI INGEGNOSI A SEMPLICI TECNICHE DI ANALISI 1.
#PER CAPIRE QUALI DOBBIAMO PARLARE DEL DOMINIO.
# DI CHE LA MATEMATICA è CASO COMPLICATO BLABLA NEI CAMBIAMENTI DI COORDINATE

class Dubbio(ThreeDScene):
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
      self.move_camera(phi=70*DEGREES,theta=-37*DEGREES)
      axes = ThreeDAxes()
      grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
      grilla2=grilla.copy().next_to(grilla,UP,buff=0)
      grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
      grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
      grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
      grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
      grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
      grilla8=grilla.copy().next_to(grilla,UP+RIGHT,buff=0)
      p=VGroup(*list(VGroup(*list(Prism(dimensions=[1/5, 1/5, np.cos(1+i/5)*np.sin(1+j/5)+2], stroke_width=0) for i in range (0,11))) for j in range (0,11)))
      for i in range (0,11):
        for j in range (0,11): 
          p[j][i].move_to((1+i/5,1+j/5,(np.cos(1+i/5)*np.sin(1+j/5)+2)/2))
      p.set_color_by_gradient(ORANGE,PURPLE)
      p.move_to(2*DOWN+1*RIGHT)
      Area=TexMobject(r"\iint f(x,y)dxdy=^{?}V_1+V_2+V_3+\\\\V_4+V_5+V_6+V_7+...").scale(0.7).move_to(3*RIGHT+3*UP+1*Z_AXIS).rotate(PI/2,axis=X_AXIS).rotate(PI/3,axis=Z_AXIS)
      Area[13:37].set_color_by_gradient(ORANGE,PURPLE)
      #Area[0:15].set_color(WHITE)
      palabras_ale = TexMobject(r"\text{Base $\times$ Height infinite times?!}").to_edge(UP)
      palabras_ale[0:11].set_color_by_gradient(ORANGE,PURPLE)
      group = VGroup(axes,p,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8,Area)
      group.move_to(3*LEFT+2*DOWN)
      self.add(axes,p,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,grilla8,Area[0:13], palabras_ale)
      Ale=Alex().to_corner(DR)
      Ale[2:4].shift(0.08*LEFT)
      teorema = TextMobject("Teorema fondamentale \\\\ del Calcolo Integrale!")
      Ale[4].set_color(BLUE)
      fras=VGroup(Ale,palabras_ale,teorema)
      self.add_fixed_in_frame_mobjects(fras)

      self.add(Ale)
      for i in range(0,7): 
         self.play(p[i][10].scale, 1.2)
         self.play(p[i][10].scale, 0.8333, TransformFromCopy(p[i][10],Area[13+3*i:16+3*i]))
      self.play(Write(Area[34:37]),Blink(Ale))
      self.play(FadeOut(palabras_ale), Blink(Ale))
      self.wait(2)
      self.play(Blink(Ale))
      self.wait(2)
      self.play(Blink(Ale))
      self.wait(2)         
      
      class Jacobiano(Scene):
    def construct(self):
        mat = [[1.0, 0.0],[1.0, 0.5]]
        square = Square()
        anno = TextMobject(r"Transformation")
        anno.shift(2 * DOWN)
        self.add(anno)
        self.add(square)
        self.play(ApplyMatrix(mat, square))

class Circonf(ThreeDScene):
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
        axes = ThreeDAxes()
        grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
        self.add(axes,grilla)
        self.wait(1)
        c=Circle(radius=1.8).set_fill(color=BLUE,opacity=.6)
        c.move_to((2.5,2.1,0))
        c.set_color_by_gradient(PURPLE,ORANGE)
        self.play(DrawBorderThenFill(c))
        xc=DashedLine((2.5,0,0),(2.5,2.1,0)).set_color(YELLOW)
        x=TexMobject(r"x_c").next_to(xc,DOWN).set_color(YELLOW)
        yc=DashedLine((0,2.1,0),(2.5,2.1,0)).set_color(YELLOW)
        y=TexMobject(r"y_c").next_to(yc,LEFT).set_color(YELLOW)
        self.play(ShowCreation(xc),ShowCreation(yc),FadeIn(y),FadeIn(x))
        self.wait(1)
        def line(dr=PI/6):
                return Line((2.5,2.1,0),(2.5+1.8*np.cos(dr),2.1+1.8*np.sin(dr),0)).set_color(MAROON)
        l=line()
        def update_line(l,alpha):
                dr=interpolate(PI/6,TAU+PI/6,alpha)
                ll=line(dr)
                l.become(ll)
        r=TexMobject(r"R").set_color(MAROON).move_to(l.get_center()+.4*UP)
        self.play(ShowCreation(l),FadeIn(r))
        r.add_updater(lambda m: m.move_to(l.get_center()+.4*UP))
        self.play(UpdateFromAlphaFunc(l,update_line),run_time=3,rate_func=linear)
        self.wait(1)

        def puntox(t=PI/6):
         return DashedLine((2.5+1.8*np.cos(t),0,0),(2.5+1.8*np.cos(t),2.1+1.8*np.sin(t),0)).set_color(ORANGE)
        px=puntox()
        def update_puntox(px,alpha):
         t=interpolate(PI/6,TAU+PI/6,alpha)
         ppx=puntox(t)
         px.become(ppx)
        def puntoy(t2=PI/6): 
         return DashedLine((0,2.1+1.8*np.sin(t2),0),(2.5+1.8*np.cos(t2),2.1+1.8*np.sin(t2),0)).set_color(ORANGE)
        py=puntoy()
        def update_puntoy(py,alpha):
         t2=interpolate(PI/6,TAU+PI/6,alpha)
         ppy=puntoy(t2)
         py.become(ppy)
        punto=TexMobject(r"(x,y)").next_to(px,UP).shift(.4*RIGHT)
        punto[1].set_color(ORANGE)
        punto[3].set_color(ORANGE)
        def linex(dx=PI/6):
                return Line((2.5,2.1,0),(2.5+1.8*np.cos(dx),2.1,0)).set_color(RED)
        lx=linex()
        def liney(dy=PI/6):
                return Line((2.5+1.8*np.cos(dy),2.1,0),(2.5+1.8*np.cos(dy),2.1+1.8*np.sin(dy),0)).set_color(RED)
        ly=liney()
        def update_linex(lx,alpha):
         dx=interpolate(PI/6,TAU+PI/6,alpha)
         llx=linex(dx)
         lx.become(llx)  
        def update_liney(ly,alpha):
         dy=interpolate(PI/6,TAU+PI/6,alpha)
         lly=liney(dy)
         ly.become(lly)   
        cos=TexMobject(r"R\cos\theta").move_to(lx.get_center()+.2*DOWN).scale(.7)
        cos.add_updater(lambda m: m.move_to(lx.get_center()+.2*DOWN))
        cos[0].set_color(MAROON)
        cos[1:5].set_color(RED)
        self.play(FadeOut(r),ShowCreation(px),ShowCreation(py),FadeIn(punto))
        self.wait(1)
        self.play(TransformFromCopy(l,lx),FadeIn(cos))
        sin=TexMobject(r"R\sin\theta").move_to(ly.get_center()+.55*RIGHT).scale(.7)
        sin.add_updater(lambda m: m.move_to(ly.get_center()+.55*RIGHT))
        sin[0].set_color(MAROON)
        sin[1:5].set_color(RED)
        self.play(TransformFromCopy(l,ly),FadeIn(sin))
        self.wait(1)
        theta=Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.23*UP).set_color(YELLOW).fade(.7)
        TH=TexMobject(r"\theta").set_color(RED).scale(.7).move_to(theta.get_center()+.4*RIGHT)
       # theta.add_updater(lambda m: m.become(Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.22*UP).set_color(YELLOW).fade(.7)))
       # TH.add_updater(lambda m: m.move_to(theta.get_center()+.5*UP))
        self.play(ShowCreation(theta),FadeIn(TH))
        self.wait(1)
        self.play(FadeOut(theta),FadeOut(TH))
        self.play(UpdateFromAlphaFunc(px,update_puntox),UpdateFromAlphaFunc(py,update_puntoy),UpdateFromAlphaFunc(l,update_line),UpdateFromAlphaFunc(lx,update_linex),UpdateFromAlphaFunc(ly,update_liney),run_time=5,rate_func=linear) 
        self.wait(1) 
        ly2=ly.copy()
        lx2=lx.copy()
        xc2=xc.copy()
        yc2=yc.copy()
        self.play(yc2.shift,(2.1)*DOWN,lx2.shift,2.1*DOWN)
        xx=VGroup(yc2,lx2,x)
        b=Brace(mobject=xx,direction=DOWN).set_color(ORANGE)
        t=b.get_tex(r"x=x_c+R\cos\theta")
        t[2:4].set_color(YELLOW)
        t[0].set_color(ORANGE)
        t[5].set_color(MAROON)
        t[6:10].set_color(RED)
        self.play(GrowFromCenter(b))
        self.play(TransformFromCopy(punto[1],t[0]))
        self.play(ShowCreation(t[1]))
        self.play(TransformFromCopy(x[0],t[2]),TransformFromCopy(x[1],t[3]))
        self.play(Write(t[4]))
        self.play(TransformFromCopy(cos[0],t[5]),TransformFromCopy(cos[1],t[6]),TransformFromCopy(cos[2],t[7]),TransformFromCopy(cos[3],t[8]),TransformFromCopy(cos[4],t[9]))
        self.wait(1)
        self.play(xc2.shift,(2.5)*LEFT,ly2.shift,(2.5+1.8*np.cos(PI/6))*LEFT)
        yy=VGroup(xc2,ly2,y)
        b2=Brace(mobject=yy,direction=LEFT).set_color(ORANGE)
        t2=b2.get_tex(r"y=y_c+R\sin\theta")
        t2[2:4].set_color(YELLOW)
        t2[0].set_color(ORANGE)
        t2[5].set_color(MAROON)
        t2[6:10].set_color(RED)
        self.play(GrowFromCenter(b2))
        self.play(TransformFromCopy(punto[3],t2[0]))
        self.play(ShowCreation(t2[1]))
        self.play(TransformFromCopy(y[0],t2[2]),TransformFromCopy(y[1],t2[3]))
        self.play(Write(t2[4]))
        self.play(TransformFromCopy(sin[0],t2[5]),TransformFromCopy(sin[1],t2[6]),TransformFromCopy(sin[2],t2[7]),TransformFromCopy(sin[3],t2[8]),TransformFromCopy(sin[4],t2[9]))
        self.wait(1)
        self.play(t.move_to,4*LEFT+3*UP,t2.move_to,4*LEFT+2*UP)
        T=VGroup(t,t2)
        bb=Brace(mobject=T,direction=LEFT)
        self.play(GrowFromCenter(bb))
        br=VGroup(bb,t,t2)
        R=SurroundingRectangle(br)
        self.play(ShowCreationThenDestruction(R))
        self.wait(1)
        gruppone=VGroup(lx,ly,lx2,ly2,xc,yc,x,y,xc2,yc2,l,punto,b,b2,cos,sin,px,py)
        self.play(FadeOut(gruppone))
        self.wait(1)
        C=c.copy().set_color_by_gradient(ORANGE,PURPLE)
        L=l.copy().set_color(BLUE)
        self.play(GrowFromCenter(C,run_time=4),ShowCreation(L,run_time=4))
        planedot=VGroup(*list(VGroup(*list(Dot((2.5+1.8*j/5*np.cos(TAU*i/25),2.1+1.8*j/5*np.sin(TAU*i/25),0)).scale(.5) for i in range (0,26))) for j in range (0,6))).set_color(PURPLE)
        self.play(FadeOut(C),FadeOut(L),FadeOut(c))
        self.play(GrowFromCenter(planedot))
        self.wait(1)
        self.play(FadeToColor(t[0],PURPLE),FadeToColor(t2[0],PURPLE))
        linea=Line(planedot[0][0],planedot[3][3]).set_color(BLUE)
        rho=TexMobject(r"\rho").next_to(linea).set_color(BLUE)
        self.play(ShowCreation(linea),FadeIn(rho))
        rho2=rho.copy()
        rho2.move_to(t[5].get_center()+.1*DOWN)
        rho3=rho.copy()
        rho3.move_to(t2[5].get_center()+.1*DOWN)
        rho.add_updater(lambda m: m.next_to(linea))
        self.play(Transform(t[5],rho2),Transform(t2[5],rho3))
        for i in range (0,4):
                   nuovalinea=Line(planedot[0][0],planedot[i+1][i+10]).set_color(BLUE)
                   self.play(Transform(linea,nuovalinea))
                   self.wait(.3)
        sep=Rectangle(width=.15,height=8.5, color=WHITE, stroke_width=0).next_to(axes,buff=2.2).set_fill(GREY, opacity=1)
        axes2=ThreeDAxes(x_min=-.9).next_to(sep,buff=0)
        rho4=TexMobject(r"\theta").set_color(GREY).move_to(axes2).shift(3.5*UP+2*LEFT)
        theta2=TexMobject(r"\rho").set_color(GREY).next_to(axes2,buff=0).shift(.6*LEFT+.2*UP)
        R2=TexMobject(r"R").set_color(MAROON).move_to((4+9,-.3,0))
        tau=TexMobject(r"2\pi").set_color(GREEN).move_to((-.4+9,3,0))
        self.add(axes2,rho4,theta2,R2,tau)
        testo=TextMobject("Polar Coordinates").scale(1.2).move_to((12.25,3.5,0)).set_color(GREY)
        GRUPPONE1=VGroup(linea,planedot,axes,sep,axes2,rho4,theta2,R2,tau)
        GRUPPONE2=VGroup(sep,axes2,rho4,theta2,R2,tau,testo)
        self.play(GRUPPONE1.shift,6*LEFT,GRUPPONE2.shift,8*LEFT,br.shift,5*DOWN+1*RIGHT,rho2.shift,5*DOWN+1*RIGHT,rho3.shift,5*DOWN+1*RIGHT,run_time=2)
        self.wait(1)
        self.play(FadeOut(rho),FadeOut(linea))
        planedot2=VGroup(*list(VGroup(*list(Dot((4*i/25,3*j/5,0)).scale(.5) for i in range (0,26))) for j in range (0,6))).set_color(PURPLE).shift(1*RIGHT)
        self.play(TransformFromCopy(planedot,planedot2),run_time=3)
        self.wait(1)
        rett=Rectangle(width=.01,height=3,color=PURPLE).move_to(planedot2).set_fill(PURPLE, opacity=.5).shift(1.99*LEFT)
        rett2=Rectangle(width=4,height=3,color=PURPLE).move_to(planedot2).set_fill(PURPLE, opacity=.5)
        C2=C.copy().move_to(planedot)
        self.play(FadeOut(planedot),FadeOut(planedot2),FadeIn(rett))
        self.play(GrowFromCenter(C2,run_time=4),Transform(rett,rett2,run_time=4))
        self.wait(2)
        def ano(dtheta=TAU):
         return Sector(inner_radius=1,outer_radius=1.8,angle=dtheta,color=PURPLE,stroke_width=4).move_to(C2).set_fill(PURPLE, opacity=.5)
        cc=ano()
        rettcc=Rectangle(width=1.6,height=3,color=PURPLE).move_to(rett2).set_fill(PURPLE, opacity=.5).shift(1.2*RIGHT)
        def update_theta(cc,alpha):
         dtheta=interpolate(TAU,TAU-PI/1.7,alpha)
         dt=ano(dtheta)
         cc.become(dt)
        oR=Line((2.5,2.1,0),(2.5+1.8,2.1,0)).set_color(BLUE).shift(6*LEFT)
        iR=Line((2.5,2.1,0),(2.5,3.075,0)).set_color(BLUE).shift(6*LEFT)
        rho1=TexMobject(r"\rho_1").set_color(BLUE).next_to(iR,LEFT)
        rho11=rho1.copy().next_to(R2,LEFT,buff=1.2)
        rr=TexMobject(r"\rho_2").set_color(BLUE).next_to(oR,DOWN)
        rrr=rr.copy().move_to(R2)
        self.play(ReplacementTransform(C2,cc),Transform(rett,rettcc),run_time=2)
        self.play(ShowCreation(oR),ShowCreation(iR))
        self.play(FadeIn(rho1),FadeIn(rho11),FadeIn(rr),Transform(R2,rrr))
        self.wait(1)
        bastah=Rectangle(width=1.6,height=2,color=PURPLE).move_to(rett2).set_fill(PURPLE, opacity=.5).shift(1.2*RIGHT+.5*DOWN)
        dl=DashedLine((2.5,2.075,0),(2.5+1.8*np.cos(TAU-PI/1.7),2.1+1.8*np.sin(TAU-PI/1.7),0)).set_color(YELLOW).shift(6*LEFT)
        angl=Sector(outer_radius=.28,angle=TAU-PI/1.7).set_color(YELLOW).fade(.7).move_to(c.get_center()+.5*RIGHT).shift(6.5*LEFT+.05*DOWN)
        self.play(iR.shift,0.025*DOWN,oR.shift,0.05*DOWN,UpdateFromAlphaFunc(cc,update_theta),Transform(rett,bastah),run_time=2)
        ddl=DashedLine((0,2,0),(4,2,0)).set_color(YELLOW).next_to(bastah,LEFT,buff=0).shift(UP+1.59*RIGHT)
        self.play(ShowCreation(dl),ShowCreation(ddl))
        self.play(ShowCreation(angl), run_time=1.5)
        thth=TexMobject(r"\theta").set_color(YELLOW).next_to(angl,LEFT)
        tthh=thth.copy().next_to(ddl,LEFT)
        self.play(FadeIn(thth),Transform(tau,tthh))
        self.wait(1)
        #IT'S NOT IMPORTANT WHICH VALUE OF r and THETA WE CHOOSE, UNTIL WE ARE WITHIN THE RANGE THAT DEFINES THE CIRC, WE WILL STILL BE INSIDE THE CIRC. BUT WHAT DOES THIS REMINDS YOU?
        #PARLA DELLA FIGURE CHE CAMBIANO

class NonJ(Scene):
    def construct(self):
        a=TexMobject(r"\int\int_D f(x,y)dxdy=\int_{\rho_1}^{\rho_2}\int_{\theta_1}^{\theta_2}f(\rho,\theta)d\rho d\theta").scale(1.5).shift(.5*LEFT)
        a[5].set_color(PURPLE)
        a[7].set_color(GREEN)
        a[10].set_color(PURPLE)
        a[12].set_color(GREEN)
        a[15:19].set_color(BLUE)
        a[20:24].set_color(YELLOW)
        a[26].set_color(BLUE)
        a[28].set_color(YELLOW)
        a[31].set_color(BLUE)
        a[33].set_color(YELLOW)
        a[0].shift(.5*RIGHT)
        a[14:19].shift(.5*RIGHT)
        a[14:35].shift(.5*LEFT)
        self.play(FadeInFromDown(a[0:13]))
        self.wait(1)
        self.play(ShowCreation(a[13]))
        self.play(Write(a[14:35]))
        self.wait(2)
        x=ImageMobject("x").move_to(a).set_color(RED).scale(5)
        self.add(x)
        self.wait(1)
        self.play(FadeOut(x))
        self.wait(1)
        j=TexMobject(r"\rho\rho\,\,d \rho d \theta").scale(1.5).next_to(a[29],buff=SMALL_BUFF)
        nuovafunz=VGroup(a[24:30],j[0:2])
        br=Brace(mobject=nuovafunz,direction=DOWN).set_color(YELLOW)
        t=br.get_text("New function to integrate \\\\ in the new Domain").set_color_by_gradient(BLUE,GREEN)
        j[1].set_color(BLUE)
        j[3].set_color(BLUE)
        j[5].set_color(YELLOW)
        c=Dot().move_to(j[0].get_center()+.1*UP).scale(.7)
        self.play(Transform(a[30:34],j))
        r=SurroundingRectangle(j[1:6]).set_color(GREEN)
        self.play(ShowCreationThenDestruction(r))
        self.wait(1)
        self.play(FadeIn(c),GrowFromCenter(br))
        self.play(Write(t))
        self.wait(1)
        #we are missing a big part here, remember when in single integral we used the u-substtution? we make a similar thing
        

class CM(ThreeDScene):
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
         axes = ThreeDAxes()
         grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
         grilla2=grilla.copy().next_to(grilla,UP,buff=0)
         grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
         grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
         grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
         grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
         grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
         self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
         self.play(ShowCreation(grilla))
         self.wait(1)            
         plane=ParametricSurface(
               lambda u,v : np.array([
                v*np.cos(u),
                v*np.sin(u),
                0,
            ]),v_min=0,v_max=2.5,u_min=0,u_max=TAU,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).set_color(GREY)
         plane.set_colors_by_radial_gradient(inner_color=YELLOW,outer_color=ORANGE,radius=2.1)
         plane.save_state()
         plane.set_color(GREY)
         self.play(GrowFromCenter(plane))#un disco fatto di diversi tipi di metallo
         self.wait(1)
         self.play(Restore(plane,run_time=3))
         self.wait(1)
         lineev=VGroup(*list(Line((-2.5+5*i/20,-np.sqrt(2.5**2-(-2.5+5*i/20)**2),0),(-2.5+5*i/20,+np.sqrt(2.5**2-(-2.5+5*i/20)**2),0)) for i in range (0,20))).set_color(BLACK).fade(.6)
         lineeo=VGroup(*list(Line((-np.sqrt(2.5**2-(-2.5+5*i/20)**2),-2.5+5*i/20,0),(np.sqrt(2.5**2-(-2.5+5*i/20)**2),-2.5+5*i/20,0)) for i in range (0,20))).set_color(BLACK).fade(.6)
         self.play(ShowCreation(lineev),ShowCreation(lineeo))
         self.wait(1)
         ds=Square(side_length=1/4,stroke_width=.1).move_to(lineev[15]).shift(3.5/4*UP+1/8*RIGHT).set_color(PURPLE).set_fill(PURPLE, opacity=.5)
         self.play(DrawBorderThenFill(ds))
         dS=TexMobject(r"dS=dxdy").next_to(ds).set_color(PURPLE).shift(RIGHT)
         d=VGroup(ds,dS)
         self.play(Write(dS[0:2]))
         self.play(ShowCreation(dS[2]))
         self.play(Write(dS[3:7]))
         self.wait(1)  #HOW MANY MASS A DS CONTAINS DEPENDS ON WHERE THAT FRACTION IS LOCATED, IN OURE EXAMPLE, IS CONVENIENT TO TURN IN CARTESIAN COORDINATES
         self.play(d.shift,1*LEFT+1*DOWN)
         self.play(ds.fade,.8)
         self.play(d.shift,2*LEFT+1*DOWN)
         self.play(ds.fade,.1)
         self.wait(1)
         m=TexMobject(r"dm=\sigma(x,y)dS").to_corner(UL)
         m[0:2].set_color(MAROON)
         m[3:9].set_color(BLUE)
         m[9:11].set_color(PURPLE)
         self.play(FadeInFromDown(m[0:2]))
         self.play(ShowCreation(m[2]))
         self.play(Write(m[3:11],run_time=2))
         self.wait(1)
         rho=TexMobject(r"\sigma(x,y)=\sqrt{x^2+y^2}\,\frac{Kg}{m^3}").to_corner(UL).set_color_by_gradient(GREEN,BLUE)
         self.play(FadeOut(lineeo),FadeOut(lineev),FadeOut(d),Write(axes))
         self.wait(1)
         def line(dr=PI/6):
                return Line((0,0,0),(2.5*np.cos(dr),2.5*np.sin(dr),0)).set_color(MAROON)
         l=line()
         def update_line(l,alpha):
                dr=interpolate(PI/6,TAU+PI/6,alpha)
                ll=line(dr)
                l.become(ll)
         r=TexMobject(r"\sqrt{x^2+y^2}").set_color(MAROON).move_to(l.get_center()+.4*UP).rotate(PI/6,axis=Z_AXIS).scale(.7)
         self.play(ShowCreation(l,run_time=3)) #puoi metterci run_time diversi nella stessa animazione!
         def puntox(t=PI/6):
          return DashedLine((2.5*np.cos(t),0,0),(2.5*np.cos(t),2.5*np.sin(t),0)).set_color(WHITE)
         px=puntox()
         def update_puntox(px,alpha):
          t=interpolate(PI/6,TAU+PI/6,alpha)
          ppx=puntox(t)
          px.become(ppx)
         def puntoy(t2=PI/6): 
          return DashedLine((0,2.5*np.sin(t2),0),(2.5*np.cos(t2),2.5*np.sin(t2),0)).set_color(WHITE)
         py=puntoy()
         def update_puntoy(py,alpha):
          t2=interpolate(PI/6,TAU+PI/6,alpha)
          ppy=puntoy(t2)
          py.become(ppy)
         punto=TexMobject(r"(x,y)").next_to(px,UP).shift(.4*RIGHT)
         punto[1].set_color(ORANGE)
         punto[3].set_color(ORANGE)
         cos=TexMobject(r"y").move_to(py.get_center()+.25*RIGHT).scale(.7)
         cos.add_updater(lambda m: m.move_to(px.get_center()+.25*RIGHT))
         self.play(ShowCreation(px),ShowCreation(py),FadeIn(punto))
         self.wait(1)
         self.play(FadeIn(cos))
         sin=TexMobject(r"x").move_to(px.get_center()+.2*UP).scale(.7)
         sin.add_updater(lambda m: m.move_to(py.get_center()+.2*UP))
         self.play(FadeIn(sin))
         self.wait(1)
         #theta=Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.23*UP).set_color(YELLOW).fade(.7)
         #TH=TexMobject(r"\theta").set_color(RED).scale(.7).move_to(theta.get_center()+.4*RIGHT)
       #  theta.add_updater(lambda m: m.become(Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.22*UP).set_color(YELLOW).fade(.7)))
       # TH.add_updater(lambda m: m.move_to(theta.get_center()+.5*UP))

         self.play(UpdateFromAlphaFunc(px,update_puntox),UpdateFromAlphaFunc(py,update_puntoy),UpdateFromAlphaFunc(l,update_line),run_time=3,rate_func=linear)
         self.wait(1)
         self.play(FadeIn(r,run_time=2),Uncreate(px),Uncreate(py),FadeOut(sin),FadeOut(cos))
         self.play(FadeOut(m))
         self.play(FadeInFromDown(rho))
         M=TexMobject(r"M=\int\int \sqrt{x^2+y^2}dxdy").move_to(rho.get_center()+1*DOWN+.4*LEFT)
         M[3:15].shift(.3*LEFT)
         M[0].set_color(MAROON)
         M[4:11].set_color(BLUE)
         M[11:15].set_color(PURPLE)
         self.play(Write(M,run_time=3))
         self.wait(1)
         camb=TexMobject(r"\int_0^{2\pi}\int_0^1\cancel{\sqrt{\rho^{\cancel{2}}}}}\rho d\rho d\theta").next_to(M[1],buff=0)#parentesi graffe anche se fai cancel
         camb[0:4].shift(.3*RIGHT)
         camb[1:4].set_color(YELLOW)
         camb[5:7].set_color(BLUE)
         camb[7:10].set_color(RED)
         camb[13].set_color(RED)
         self.play(ReplacementTransform(M[2:4],camb[0:7]),M[4:15].next_to,camb[4])
         self.play(ReplacementTransform(M[4:10],camb[10:13]),ReplacementTransform(M[10],camb[14]))
         self.play(ReplacementTransform(M[11:15],camb[15:20]))
         self.wait(1)
         self.play(ShowCreation(camb[13]),ShowCreation(camb[7:10]))
         self.play(FadeOut(camb[7:12]),FadeOut(camb[13:15]),camb[12].shift,.3*LEFT)
         self.wait()
         self.play(camb[15].move_to,camb[12],camb[16:20].next_to,camb[12])
         self.play(ShowCreation(camb[14]))
         self.wait(1)
         ris=TexMobject(r"=\frac{2}{3}\pi").move_to(M.get_center()+1.5*DOWN+2*LEFT).set_color(MAROON)
         self.play(ShowCreation(ris[0]))
         self.play(Write(ris[1:5]))
         ris2=TexMobject(r"\approx 2,09 Kg").next_to(ris).set_color_by_gradient(MAROON,BLUE)
         self.play(Write(ris2))
         self.wait(1)
         self.play(FadeOut(r),FadeOut(ris),FadeOut(ris2),FadeOut(M[0:2]),FadeOut(camb[0:7]),FadeOut(camb[12]),FadeOut(camb[14:20]),FadeOut(rho),FadeOut(l),FadeOut(punto))
         self.play(FadeToColor(plane,GREY))
         #since the domain is a circle, we can use a change of coordinates, and since this circle is the same of the previous example, we use the same transformation
         #FAI VEDERE RISOLUZIONE, E POI DIMOSTRA CHE VOLUME è IN REALTà LA MASSA
         self.move_camera(phi=70*DEGREES,theta=20*DEGREES)
         self.begin_ambient_camera_rotation(rate=0.01)
         self.wait(1)
         density=ParametricSurface(
              lambda u,v : np.array([
                v*np.cos(u),
                v*np.sin(u),
                v,
            ]),v_min=0,v_max=2.5,u_min=0,u_max=TAU,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).set_colors_by_radial_gradient(inner_color=YELLOW,outer_color=RED,radius=2.1)
         simbol=TexMobject(r"z=\sigma(x,y)").shift(2*Z_AXIS+2*UP).rotate(PI/2,axis=X_AXIS).rotate(PI/2,axis=Z_AXIS)
         simbol[2:8].set_color(BLUE)
         self.play(TransformFromCopy(plane,density,run_time=2))
         #self.play(Write(simbol))
         self.wait(1)

         def follow(dx=0): 
          return DashedLine((0,0,dx*np.sqrt(2)),(dx,-dx,dx*np.sqrt(2))).set_color(WHITE)
         f=follow()
         def update_follow(f,alpha):
          dx=interpolate(0,2.5*np.sqrt(2)/2,alpha)
          ff=follow(dx)
          f.become(ff)
         def followsu(dr=0): 
          return DashedLine((dr,-dr,0),(dr,-dr,dr*np.sqrt(2))).set_color(WHITE)
         fsu=followsu()
         def update_followsu(fsu,alpha):
          dr=interpolate(0,2.5*np.sqrt(2)/2,alpha)
          ffsu=followsu(dr)
          fsu.become(ffsu)
         L=Line((0,0,0),(2.5*np.sqrt(2)/2,-2.5*np.sqrt(2)/2,0)).set_submobject_colors_by_gradient(YELLOW,ORANGE)
         self.play(UpdateFromAlphaFunc(f,update_follow),UpdateFromAlphaFunc(fsu,update_followsu),ShowCreation(L),run_time=4)
         self.play(FadeOut(L),FadeOut(f),FadeOut(fsu))
         self.wait(1)
         g2= VGroup(*list(ParametricFunction(lambda u : np.array([
                 2.5*np.cos(TAU*i/500),
                 2.5*np.sin(TAU*i/500),
                 u,
               ]),color=RED,t_min=0,t_max=2.5,
             ) for i in range (0,500))).set_color(ORANGE)
         #metti il prisma?
         
         #self.play(FadeIn(g2),FadeOut(simbol))
         #self.wait(2)
         #self.play(g2.fade,.7)
         Mm=TexMobject(r"M=\int\int \sigma(x,y)dxdy")
         self.add_fixed_in_frame_mobjects(Mm)
         Mm.to_corner(UL).shift(.2*LEFT)
         Mm[3:14].shift(.3*LEFT)
         Mm[0].set_color(MAROON)
         Mm[4:10].set_color(BLUE)
         Mm[10:14].set_color(PURPLE)
         self.play(FadeInFromDown(Mm))
         self.wait(3)

class JJ(ThreeDScene):
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
         axes = ThreeDAxes()
         grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
         grilla2=grilla.copy().next_to(grilla,UP,buff=0)
         grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
         grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
         grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
         grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
         grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
         self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
         self.move_camera(phi=70*DEGREES,theta=-20*DEGREES)
         self.wait(1) 
         nonj=ParametricSurface(
              lambda u,v : np.array([
                u,
                v,
                4*np.exp(-(u**2+v**2)),
            ]),v_min=-.1,v_max=TAU+.1,u_min=-.1,u_max=4.1,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63))
         circle=Circle(radius=2,color=PURPLE).move_to((2,2,0)).set_fill(PURPLE,opacity=.5)
         #######################################################
         intenonj1= VGroup(*list(ParametricFunction(lambda u : np.array([
                 2+2*np.cos(TAU*i/500),
                 2+2*np.sin(TAU*i/500),
                 u,
               ]),color=RED,t_min=0,t_max=4*np.exp(-(2+2*np.cos(TAU*i/500))**2-(2+2*np.sin(TAU*i/500))**2),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonj2= VGroup(*list(ParametricFunction(lambda u : np.array([
                 2+2*np.cos(TAU*i/500),
                 u,
                 4*np.exp(-(2+2*np.cos(TAU*i/500))**2-(u)**2),
               ]),color=RED,t_min=-np.sqrt(4-(2*np.cos(TAU*i/500)**2)),t_max=+np.sqrt(4-(2*np.cos(TAU*i/500)**2)),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonj=VGroup(intenonj1,intenonj2)
#######################################################################
         x=TexMobject(r"x").move_to((5,-.3,0))
         y=TexMobject(r"y").move_to((.3,5,0)).scale(1.2)
         funznonj=TexMobject(r"\int\int e^{-(x^2+y^2)}dxdy").move_to((0,-2,1.5)).rotate(PI/2,axis=Z_AXIS).rotate(PI/2,axis=Y_AXIS)
         funznonj[0].shift(.3*UP)
         self.add(axes,grilla,grilla2,grilla3,grilla4,grilla5,grilla6,grilla7,nonj,circle,intenonj,x,y,funznonj)
         self.wait(1)
         square=Rectangle(height=TAU,width=2,color=PURPLE).move_to((1,PI,0)).set_fill(PURPLE,opacity=.5)
         nonjchange=ParametricSurface(
              lambda u,v : np.array([
                u,
                v,
                4*np.exp(-u*2),
            ]),v_min=-.1,v_max=TAU+.1,u_min=-.1,u_max=4.1,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63))
###################################################à

         intenonjchange1=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 0,
                 u,
               ]),color=RED,t_min=0,t_max=4*np.exp(-(2*i/500)**2),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonjchange2=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 TAU,
                 u,
               ]),color=RED,t_min=0,t_max=4*np.exp(-(2*i/500)**2),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonjchange3=VGroup(*list(ParametricFunction(lambda u : np.array([
                 0,
                 TAU*i/500,
                 u,
               ]),color=RED,t_min=0,t_max=4,
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonjchange4=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2,
                 TAU*i/500,
                 u,
               ]),color=RED,t_min=0,t_max=4,
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intenonjchange5=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 u,
                 4*np.exp(-(2*i/500)**2),
               ]),color=RED,t_min=0,t_max=TAU,
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)

         intenonjchange=VGroup(intenonjchange1,intenonjchange2,intenonjchange3,intenonjchange4,intenonjchange5)
###################################################################
         rho=TexMobject(r"\rho").move_to(x)
         theta=TexMobject(r"\theta").move_to(y).scale(1.2)
         funznonjchange=TexMobject(r"\int_0^{2\pi}\int_0^2 e^{-\rho^2}d\rho d\theta").move_to(funznonj).rotate(PI/2,axis=Z_AXIS).rotate(PI/2,axis=Y_AXIS)
         funznonjchange[0:4].shift(.3*UP)
         self.play(Transform(funznonj,funznonjchange),Transform(circle,square),ReplacementTransform(nonj,nonjchange,run_time=3),ReplacementTransform(intenonj,intenonjchange,run_time=3),Transform(x,rho),Transform(y,theta))
         self.wait(2)
         jchange=ParametricSurface(
              lambda u,v : np.array([
                u,
                v,
                4*u*np.exp(-u*2),
            ]),v_min=-.1,v_max=4.1,u_min=-.1,u_max=4.1,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63))
         intejchange1=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 0,
                 u,
               ]),color=RED,t_min=0,t_max=4*(2*i/500)*np.exp(-(2*i/500)**2),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intejchange2=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 TAU,
                 u,
               ]),color=RED,t_min=0,t_max=4*(2*i/500)*np.exp(-(2*i/500)**2),
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
        # intejchange3=VGroup(*list(ParametricFunction(lambda u : np.array([
        #         0,
        #         TAU*i/500,
        #         u,
        #       ]),color=RED,t_min=0,t_max=0,
        #     ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
        #intejchange4=VGroup(*list(ParametricFunction(lambda u : np.array([
        #         2,
        #         TAU*i/500,
        #         u,
        #       ]),color=RED,t_min=0,t_max=0,
        #     ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intejchange5=VGroup(*list(ParametricFunction(lambda u : np.array([
                 2*i/500,
                 u,
                 4*(2*i/500)*np.exp(-(2*i/500)**2),
               ]),color=RED,t_min=0,t_max=TAU,
             ) for i in range (0,500))).set_color_by_gradient(PURPLE,ORANGE)
         intejchange=VGroup(intejchange1,intejchange2,intejchange5)
         rhorho=TexMobject(r"\rho").next_to(funznonjchange[7])
         self.play(funznonjchange[11:15].shift,.4*UP,FadeInFromDown(rhorho),ReplacementTransform(nonjchange,jchange,run_time=3),ReplacementTransform(intenonjchange,intejchange,run_time=3))
         self.wait(2)

class Jacobian(Scene):
   def construct(self):
     co=TexMobject(r"\begin{cases}x=x(\rho,\theta) \\\\y=y(\rho,\theta)\end{cases}").scale(2)
     dxdy=TexMobject(r"dxdy").scale(2).move_to((-3,0,0))
     drdt=TexMobject(r"??? d\rho d\theta").scale(2).move_to((3,0,0))
     drdt[0:3].set_color_by_gradient(MAROON,RED)
     g=Arrow(dxdy,drdt).set_color(YELLOW)
     self.play(Write(co))
     self.wait(1)
     self.play(co.scale,.5)
     self.play(co.to_corner,DL)
     self.play(Write(dxdy))
     self.play(GrowArrow(g))
     self.play(Write(drdt))
     j=TextMobject("JACOBIAN").scale(3).set_color_by_gradient(MAROON,RED).move_to(2.5*UP)
     self.wait(1)
     self.play(FadeInFromLarge(j,.1))
     self.wait(1)
     self.play(j.shift,10*DOWN,FadeOut(co,run_time=.5),FadeOut(dxdy,run_time=.5),FadeOut(drdt,run_time=.5),FadeOut(g,run_time=.5))
     self.wait(1)
     J=TexMobject(r"J=\det\left(\begin{matrix}\displaystyle\frac{\partial x}{\partial \rho} & \displaystyle\frac{\partial x}{\partial \theta}\\\\\displaystyle\frac{\partial y}{\partial\rho} & \displaystyle\frac{\partial y}{\partial \theta}\end{matrix}\right)").scale(2).shift(1*LEFT)
     self.play(Write(J,run_time=5))
     co2=TexMobject(r"\begin{cases}x=x_c+\rho\cos\theta \\\\y=y_c+\rho\sin\theta\end{cases}").to_corner(UL).set_color_by_gradient(BLUE,YELLOW)
     self.wait(1)
     Ale=Alex().scale(.7)
     Ale[4].set_color(BLUE)
     Ale.move_to(4*LEFT+10*DOWN)
     self.play(Ale.to_corner,DL)
     pu=TextMobject("?").scale(1.5).next_to(Ale.get_center()+1.5*UP+.5*RIGHT)
     self.play(Blink(Ale), FadeIn(pu))
     self.play(FadeInFromDown(co2))
     u=TexMobject(r"=\rho").scale(2).next_to(J)
     self.play(Blink(Ale),ShowCreation(u[0]))
     self.play(FadeOut(pu),Write(u[1]))
     re=SurroundingRectangle(u[1])
     self.play(Blink(Ale),ShowCreationThenDestruction(re))
     self.wait(1)
     self.play(Blink(Ale))
     self.wait(1)

class CM2(ThreeDScene):
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
         axes = ThreeDAxes()
         grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
         grilla2=grilla.copy().next_to(grilla,UP,buff=0)
         grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
         grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
         grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
         grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
         grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
         self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
         self.play(ShowCreation(grilla))
         self.wait(1)            
         plane=ParametricSurface(
               lambda u,v : np.array([
                v*np.cos(u),
                v*np.sin(u),
                0,
            ]),v_min=0,v_max=2.5,u_min=0,u_max=TAU,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).set_color(GREY)
         plane.set_colors_by_radial_gradient(inner_color=YELLOW,outer_color=ORANGE,radius=2.1)
         plane.save_state()
         plane.set_color(GREY)
         self.play(GrowFromCenter(plane))#un disco fatto di diversi tipi di metallo
         self.wait(1)
         self.play(Restore(plane,run_time=3))
         self.wait(1)
         lineev=VGroup(*list(Line((-2.5+5*i/20,-np.sqrt(2.5**2-(-2.5+5*i/20)**2),0),(-2.5+5*i/20,+np.sqrt(2.5**2-(-2.5+5*i/20)**2),0)) for i in range (0,20))).set_color(BLACK).fade(.6)
         lineeo=VGroup(*list(Line((-np.sqrt(2.5**2-(-2.5+5*i/20)**2),-2.5+5*i/20,0),(np.sqrt(2.5**2-(-2.5+5*i/20)**2),-2.5+5*i/20,0)) for i in range (0,20))).set_color(BLACK).fade(.6)
         self.play(ShowCreation(lineev),ShowCreation(lineeo))
         self.wait(1)
         ds=Square(side_length=1/4,stroke_width=.1).move_to(lineev[15]).shift(3.5/4*UP+1/8*RIGHT).set_color(PURPLE).set_fill(PURPLE, opacity=.5)
         self.play(DrawBorderThenFill(ds))
         dS=TexMobject(r"dS=dxdy").next_to(ds).set_color(PURPLE).shift(RIGHT)
         d=VGroup(ds,dS)
         self.play(Write(dS[0:2]))
         self.play(ShowCreation(dS[2]))
         self.play(Write(dS[3:7]))
         self.wait(1)  #HOW MANY MASS A DS CONTAINS DEPENDS ON WHERE THAT FRACTION IS LOCATED, IN OURE EXAMPLE, IS CONVENIENT TO TURN IN CARTESIAN COORDINATES
         self.play(d.shift,1*LEFT+1*DOWN)
         self.play(ds.fade,.8)
         self.play(d.shift,2*LEFT+1*DOWN)
         self.play(ds.fade,.1)
         self.wait(1)
         m=TexMobject(r"dm=\sigma(x,y)dS").to_corner(UL)
         m[0:2].set_color(MAROON)
         m[3:9].set_color(BLUE)
         m[9:11].set_color(PURPLE)
         self.play(FadeInFromDown(m[0:2]))
         self.play(ShowCreation(m[2]))
         self.play(Write(m[3:11],run_time=2))
         self.wait(1)
         rho=TexMobject(r"\sigma(x,y)=\sqrt{x^2+y^2}\,\frac{Kg}{m^2}").to_corner(UL).set_color(BLUE)
         self.play(FadeOut(lineeo),FadeOut(lineev),FadeOut(d),Write(axes))
         self.wait(1)
         def line(dr=PI/6):
                return Line((0,0,0),(2.5*np.cos(dr),2.5*np.sin(dr),0)).set_color(MAROON)
         l=line()
         def update_line(l,alpha):
                dr=interpolate(PI/6,TAU+PI/6,alpha)
                ll=line(dr)
                l.become(ll)
         r=TexMobject(r"\sqrt{x^2+y^2}").set_color(MAROON).move_to(l.get_center()+.4*UP).rotate(PI/6,axis=Z_AXIS).scale(.7)
         self.play(ShowCreation(l,run_time=3)) #puoi metterci run_time diversi nella stessa animazione!
         def puntox(t=PI/6):
          return DashedLine((2.5*np.cos(t),0,0),(2.5*np.cos(t),2.5*np.sin(t),0)).set_color(WHITE)
         px=puntox()
         def update_puntox(px,alpha):
          t=interpolate(PI/6,TAU+PI/6,alpha)
          ppx=puntox(t)
          px.become(ppx)
         def puntoy(t2=PI/6): 
          return DashedLine((0,2.5*np.sin(t2),0),(2.5*np.cos(t2),2.5*np.sin(t2),0)).set_color(WHITE)
         py=puntoy()
         def update_puntoy(py,alpha):
          t2=interpolate(PI/6,TAU+PI/6,alpha)
          ppy=puntoy(t2)
          py.become(ppy)
         punto=TexMobject(r"(x,y)").next_to(px,UP).shift(.4*RIGHT)
         punto[1].set_color(ORANGE)
         punto[3].set_color(ORANGE)
         cos=TexMobject(r"y").move_to(py.get_center()+.25*RIGHT).scale(.7)
         cos.add_updater(lambda m: m.move_to(px.get_center()+.25*RIGHT))
         self.play(ShowCreation(px),ShowCreation(py),FadeIn(punto))
         self.wait(1)
         sin=TexMobject(r"x").move_to(px.get_center()+.2*UP).scale(.7)
         sin.add_updater(lambda m: m.move_to(py.get_center()+.2*UP))
         self.play(FadeIn(sin))
         self.play(FadeIn(cos))
         self.wait(1)
         #theta=Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.23*UP).set_color(YELLOW).fade(.7)
         #TH=TexMobject(r"\theta").set_color(RED).scale(.7).move_to(theta.get_center()+.4*RIGHT)
       #  theta.add_updater(lambda m: m.become(Sector(radius=.5,start_angle=lx.get_angle(),angle=l.get_angle()).move_to(c.get_center()+.5*RIGHT+.22*UP).set_color(YELLOW).fade(.7)))
       # TH.add_updater(lambda m: m.move_to(theta.get_center()+.5*UP))

         self.play(UpdateFromAlphaFunc(px,update_puntox),UpdateFromAlphaFunc(py,update_puntoy),UpdateFromAlphaFunc(l,update_line),run_time=3,rate_func=linear)
         self.wait(1)
         self.play(FadeIn(r,run_time=2),Uncreate(px),Uncreate(py),FadeOut(sin),FadeOut(cos))
         self.play(FadeOut(m))
         self.play(FadeInFromDown(rho))
         M=TexMobject(r"M=\int\int \sqrt{x^2+y^2}dxdy").move_to(rho.get_center()+1*DOWN+.2*LEFT)
         M[3:15].shift(.3*LEFT)
         M[0].set_color(MAROON)
         M[4:11].set_color(BLUE)
         M[11:15].set_color(PURPLE)
         self.play(Write(M,run_time=3))
         self.wait(1)
         camb=TexMobject(r"\int_0^{2\pi}\int_0^1\cancel{\sqrt{\rho^{\cancel{2}}}}}\rho d\rho d\theta").next_to(M[1],buff=0)#parentesi graffe anche se fai cancel
         camb[0:4].shift(.3*RIGHT)
         camb[1:4].set_color(YELLOW)
         camb[5:7].set_color(BLUE)
         camb[7:10].set_color(RED)
         camb[13].set_color(RED)
         co=TexMobject(r"\begin{cases}x=\rho\cos\theta & \rho \in [0,1]\\\\y=\rho\sin\theta & \theta \in [0,2\pi]\end{cases}").to_corner(DL).scale(.9).set_color_by_gradient(BLUE,YELLOW).shift(.2*LEFT)
         self.play(FadeInFromDown(co))
         self.wait(1)
         self.play(ReplacementTransform(M[2:4],camb[0:7]),M[4:15].next_to,camb[4])
         self.play(ReplacementTransform(M[4:10],camb[10:13]),ReplacementTransform(M[10],camb[14]))
         self.play(ReplacementTransform(M[11:15],camb[15:20]))
         self.wait(1)
         co2=TexMobject(r"\begin{matrix}\text{If you are curious about the calculations,} \\ \text{first we do the innermost integration }\displaystyle \int_0^1\rho^2d\rho=\frac{1}{3} \\ \text{and then the outermost } \displaystyle \int_0^{2\pi}\frac{1}{3}d\theta=\frac{2}{3}\pi \end{matrix}").scale(.5).to_corner(UR).set_color_by_gradient(BLUE,YELLOW).shift(.3*UP+.3*RIGHT)
         rarra=SurroundingRectangle(co2)
         self.play(FadeInFromDown(co))
         self.play(ShowCreation(camb[13]),ShowCreation(camb[7:10]))
         self.play(FadeOut(camb[7:12]),FadeOut(camb[13:15]),camb[12].shift,.3*LEFT)
         self.wait()
         self.play(camb[15].move_to,camb[12],camb[16:20].next_to,camb[12])
         due=camb[14].copy().next_to(camb[12],RIGHT+UP,buff=SMALL_BUFF/3)
         self.play(ShowCreation(due))
         self.wait(1)
         self.play(FadeInFromDown(co2),FadeInFromDown(rarra))
         self.wait(1)
         ris=TexMobject(r"=\frac{2}{3}\pi").move_to(M.get_center()+1.5*DOWN+1.8*LEFT).set_color(MAROON)
         self.play(ShowCreation(ris[0]))
         self.play(Write(ris[1:5]))
         ris2=TexMobject(r"\approx 2,09 Kg").next_to(ris).set_color_by_gradient(MAROON,BLUE)
         self.play(Write(ris2))
         self.wait(1)
         self.play(FadeOut(co2),FadeOut(rarra),FadeOut(co),FadeOut(due),FadeOut(r),FadeOut(ris),FadeOut(ris2),FadeOut(M[0:2]),FadeOut(camb[0:7]),FadeOut(camb[12]),FadeOut(camb[15:20]),FadeOut(rho),FadeOut(l),FadeOut(punto))
         self.play(FadeToColor(plane,GREY))
         self.wait(1)

class R(Scene):
   def construct(self):
      Ale=Alex().to_edge(DOWN, buff=1)
      Ale[4].set_color(BLUE)
      self.play(FadeInFromLarge(Ale,.1))
      ehma=TextMobject("What is all \\\\ of this for?!") #metti gli spazi tra le //// e le parole seenò non computa
      self.play(NumberCreatureSays(
            Ale, ehma, 
            bubble_kwargs = {"height" : 4, "width" : 6},
            target_mode="speaking"
      ), Blink(Ale))
      self.play(Blink(Ale))
      self.wait(5)
      self.play(Blink(Ale))
      self.wait(2)

class Thanks(Scene):
   def construct(self):
      Ale=Alex().to_edge(DOWN, buff=1)
      Ale[4].set_color(BLUE)
      ehma=TextMobject("Thank you \\\\ for watching!") #metti gli spazi tra le //// e le parole seenò non computa
      s=ImageMobject("Subscribe").to_corner(UL).shift(.5*UP)
      logo=ImageMobject("logo").next_to(s,DOWN,buff=SMALL_BUFF).shift(1*UP)
      self.add(Ale,s,logo)
      self.play(NumberCreatureSays(
            Ale, ehma, 
            bubble_kwargs = {"height" : 4, "width" : 6},
            target_mode="speaking"
      ), Blink(Ale))
      self.play(Blink(Ale))
      self.wait(5)
      self.play(Blink(Ale))
      self.wait(2)

class CM3(ThreeDScene):
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
         axes = ThreeDAxes()
         grilla=NumberPlane(width=self.width,height=self.height,rows=self.rows,columns=self.columns).set_stroke(self.grid_color,self.grid_stroke)
         grilla2=grilla.copy().next_to(grilla,UP,buff=0)
         grilla3=grilla.copy().next_to(grilla,DOWN,buff=0)
         grilla4=grilla.copy().next_to(grilla,LEFT+UP,buff=0)
         grilla5=grilla.copy().next_to(grilla,LEFT+DOWN,buff=0)
         grilla6=grilla.copy().next_to(grilla,LEFT,buff=0)
         grilla7=grilla.copy().next_to(grilla,RIGHT,buff=0)
         self.add(grilla2,grilla3,grilla4,grilla5,grilla6,grilla7)
         self.play(ShowCreation(grilla))
         self.wait(1)            
         plane=ParametricSurface(
               lambda u,v : np.array([
                v*np.cos(u),
                v*np.sin(u),
                0,
            ]),v_min=0,v_max=2.5,u_min=0,u_max=TAU,checkboard_colors=[BLUE_D,BLUE_E],
            resolution=(30, 63)).set_color(GREY)
         plane.set_colors_by_radial_gradient(inner_color=YELLOW,outer_color=ORANGE,radius=2.1)
         plane.save_state()
         plane.set_color(GREY)
         self.play(GrowFromCenter(plane))#un disco fatto di diversi tipi di metallo
         self.wait(1)
         def line(dr=PI/6):
                return Line((0,0,0),(2.5*np.cos(dr),2.5*np.sin(dr),0)).set_color(RED)
         l=line()
         def update_line(l,alpha):
                dr=interpolate(PI/6,TAU+PI/6,alpha)
                ll=line(dr)
                l.become(ll)
         r=TexMobject(r"R=1m").set_color(MAROON).move_to(l.get_center()+.4*UP).rotate(PI/6,axis=Z_AXIS).scale(.7)
         self.play(ShowCreationThenDestruction(l),FadeIn(r))
         self.play(FadeOut(r))
         self.wait(1)
         self.play(Restore(plane,run_time=3))
         self.wait(1)
         m=TexMobject(r"M=\sigma S").scale(2).to_corner(UL)
         m[0].set_color(MAROON)
         m[2].set_color(BLUE)
         m[3].set_color(PURPLE)
         self.play(FadeInFromDown(m[0]))
         self.wait(1)
         self.play(ShowCreation(m[1]))
         self.play(Write(m[2:4]))
         self.wait(1)
         xy=TexMobject(r"(x,y)").scale(2).next_to(m[2],buff=SMALL_BUFF).set_color(BLUE)
         self.play(Write(xy),m[3].shift,2.2*RIGHT+.1*DOWN)
         sigma=VGroup(xy,m[2])
         self.play(sigma.scale,1.2,run_time=.5)
         self.play(sigma.scale,0.8333,run_time=.5)
         self.wait(1)
         Ale=Alex().move_to((10,-10,0)).scale(.7).rotate(PI,axis=Y_AXIS)
         Ale[4].set_color(BLUE)
         self.play(Ale.to_corner,DR, buff=1)
         pu=TextMobject("?").scale(1.5).next_to(Ale.get_center()+1.5*UP+.5*RIGHT)
         self.play(Blink(Ale), FadeIn(pu))
         self.wait(1)
         di=TextMobject("DOUBLE INTEGRALS").scale(2).to_edge(UP).set_color_by_gradient(MAROON,ORANGE)
         k=VGroup(m,xy)
         self.play(k.to_corner,DL,FadeInFromLarge(di,.1),FadeOut(pu))
         self.wait(1)
         self.play(Blink(Ale))
         self.wait(1)
