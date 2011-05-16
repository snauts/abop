surface texmap(
float Ka = 1, Kd = .5, Ks = .01, roughness = 0.1;
color specularcolor = 1;
string texname = "";
string texmask = "";
)
{
     color Ct = Cs;
     color Ot = Os;
     float ss = s - floor(s);
     float tt = t - floor(t);
     if (texname != "" && texmask != "") {
	 Ct = color texture(texname, ss, tt);
	 Ot = color texture(texmask, ss, tt);
     }
     normal Nf = faceforward(normalize(N), I);
     Ci = Ct * (Ka * ambient() + Kd * diffuse(Nf))
     	  + specularcolor * Ks * specular(Nf, -normalize(I), roughness);
     Oi = Ot;
     Ci *= Oi;
}
