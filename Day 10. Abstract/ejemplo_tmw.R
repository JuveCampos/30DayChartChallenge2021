
# Chunk 1
library(rayshader)

# Hacemos el render del volcán
volcano %>%
  sphere_shade() %>%
  add_shadow(ray_shade(volcano,zscale=3),0.3) %>%
  plot_3d(volcano, zscale=3, fov=30)
render_movie("basic_video.mp4", title_text = "Basic rayshader plot",
             title_bar_color = "red", title_bar_alpha = 0.3)
rgl::rgl.close()


# Chunk 2
library(ggplot2)

volcano_contours = isoband::isolines(x = 1:ncol(volcano),
                                     y = 1:nrow(volcano),
                                     z = volcano,
                                     levels=seq(120,190,by=10))

contours = isoband::iso_to_sfg(volcano_contours)
sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))

ggplot(sf_contours) + geom_sf(aes(color = level))


library(rayrender)

scenelist = list()
counter = 1

for(i in 1:length(volcano_contours)) {
  heightval = as.numeric(names(volcano_contours)[i])
  uniquevals = table(volcano_contours[[i]]$id)
  for(k in 1:length(uniquevals)) {
    tempvals = volcano_contours[[i]]
    tempvals$x = tempvals$x[tempvals$id == k]
    tempvals$y = tempvals$y[tempvals$id == k]
    for(j in 1:(length(tempvals$x)-1)) {
      scenelist[[counter]] = segment(start = c(tempvals$x[j]-30,
                                               (heightval-80)/5-3,
                                               tempvals$y[j]-44),
                                     end   = c(tempvals$x[j+1]-30,
                                               (heightval-80)/5-3,
                                               tempvals$y[j+1]-44),
                                     radius = 0.3,
                                     material = diffuse(color=heat.colors(9)[i]))
      counter = counter + 1
    }
  }
}

fullscene = do.call(rbind, scenelist)

generate_ground(material = diffuse(color="grey20")) %>%
  add_object(fullscene) %>%
  render_scene(lookfrom = c(0,80,150), lookat = c(0,-1,-10), samples = 200,
               aperture = 0, fov = 25, width = 800, height = 800)


# Chunk 3

scenelist = list()
counter = 1

for(i in 1:length(volcano_contours)) {
  heightval = as.numeric(names(volcano_contours)[i])
  uniquevals = table(volcano_contours[[i]]$id)
  for(k in 1:length(uniquevals)) {
    tempvals = volcano_contours[[i]]
    tempvals$x = tempvals$x[tempvals$id == k]
    tempvals$y = tempvals$y[tempvals$id == k]
    for(j in 1:(length(tempvals$x)-1)) {
      scenelist[[counter]] = segment(start = c(tempvals$x[j]-30,
                                               (heightval-80)/5-3,
                                               tempvals$y[j]-44),
                                     end   = c(tempvals$x[j+1]-30,
                                               (heightval-80)/5-3,
                                               tempvals$y[j+1]-44),
                                     radius = 0.3,
                                     material = light(intensity = 3,
                                                      color=heat.colors(9)[i]))
      counter = counter + 1

      #Add a sphere at each corner
      scenelist[[counter]] = sphere(x = tempvals$x[j]-30,
                                    y = (heightval-80)/5-3,
                                    z = tempvals$y[j]-44,
                                    radius = 0.3,
                                    material = light(intensity = 3,
                                                     color=heat.colors(9)[i]))
      counter = counter + 1
    }
  }
}

fullscene2 = do.call(rbind, scenelist)

generate_ground(material = metal(color="grey20", fuzz=0.05)) %>%
  add_object(fullscene2) %>%
  render_scene(lookfrom = c(0,80,150), lookat = c(0,-1,-10), samples = 200,
               aperture = 0, fov=25, tonemap = "reinhold",
               width = 800, height = 800)

# Chunk 4 ----

generate_ground(material = metal(color="grey20", fuzz=0.05)) %>%
  add_object(fullscene2) %>%
  render_scene(lookfrom = c(0,80,150), lookat=c(0,-1,-10), samples = 200,
               aperture = 0, fov = 25, bloom = 5, tonemap = "reinhold",
               width = 800, height = 800)


# Chunk 5 ----

green_light = light(color="green", intensity = 3)

grid = list()
counter = 1
for(i in seq(110,250,by=10)) {
  grid[[counter]] = segment(start=c(sinpi(i/180)*40,
                                    -0.5,
                                    cospi(i/180)*40-20),
                            end = c(sinpi(i/180)*40,
                                    18.5,
                                    cospi(i/180)*40-20),
                            radius=0.25,
                            material = green_light)
  counter = counter + 1
}

green_grid_vertical = do.call(rbind, grid)

generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
  add_object(green_grid_vertical) %>%
  add_object(fullscene2) %>%
  render_scene(lookfrom = c(0,80,150),lookat = c(0,-1,-10), samples = 40,
               aperture = 0, fov = 25, bloom = 5, tonemap="reinhold",
               width=800,height=800)

# Chunk 6 ----

#Generate the horizontal grid stripes
cylinder(radius=40, z=-20,material = green_light,
         phi_min = 200, phi_max = 340, flipped = FALSE) %>%
  add_object(cylinder(radius=40, y=6,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, y=12,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, y=18,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=6,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=12,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=18,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) ->
  green_grid_horizontal

#Purple base disk
base_disk = disk(inner_radius = 60, radius=61, z=-10,
                 material = light(intensity = 3, color="purple")) %>%
  add_object(disk(inner_radius = 60, radius=61, y=-0.1, z=-10,
                  material = light(intensity = 3, color="purple"), flipped=TRUE))

generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
  add_object(green_grid_vertical) %>%
  add_object(green_grid_horizontal) %>%
  add_object(base_disk) %>%
  add_object(fullscene2) %>%
  render_scene(lookfrom = c(0,80,150), lookat=c(0,-1,-10), samples = 200,
               aperture = 0, fov = 25, bloom = 5, tonemap = "reinhold",
               width = 800, height = 800)

# Chunk 7 ----

xpos = 300 * sinpi(1:360/180)
zpos = 300 * cospi(1:360/180)

disk_height  = 6+6*sinpi(1:360/180*2)
disk_height2 = 6+6*sinpi(1:360/180*2+15/180)
disk_height3 = 6+6*sinpi(1:360/180*2-15/180)
disk_height4 = 6+6*sinpi(1:360/180*2+30/180)
disk_height5 = 6+6*sinpi(1:360/180*2-30/180)


for(i in seq(1,360,by=1)) {
  generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
    add_object(green_grid_vertical) %>%
    add_object(green_grid_horizontal) %>%
    add_object(group_objects(base_disk, group_translate = c(0,disk_height[i],0))) %>%
    add_object(group_objects(base_disk, group_translate = c(0,disk_height2[i],0))) %>%
    add_object(group_objects(base_disk, group_translate = c(0,disk_height3[i],0))) %>%
    add_object(group_objects(base_disk, group_translate = c(0,disk_height4[i],0))) %>%
    add_object(group_objects(base_disk, group_translate = c(0,disk_height5[i],0))) %>%
    add_object(fullscene2) %>%
    render_scene(lookfrom = c(xpos[i],160,zpos[i]-10),lookat = c(0,-1,-10), samples = 200,
                 aperture = 0, fov = 22, bloom = 5, tonemap = "reinhold",
                 width = 800, height = 800, filename = sprintf("neonvolcano%d",i))
}


av::av_encode_video(sprintf("neonvolcano%d.png",seq(1,360,by=1)), framerate = 30,
                    output = "neonvolcano.mp4")

