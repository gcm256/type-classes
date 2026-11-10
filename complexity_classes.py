import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Create a square figure canvas
fig, ax = plt.subplots(figsize=(9, 9))

# 1. PSPACE (Outer container)
pspace = patches.Circle(
    (0, 0), 4.0, 
    facecolor='#f3e5f5', edgecolor='#7b1fa2', linewidth=2, alpha=0.3
)
ax.add_patch(pspace)
ax.text(0, 3.5, 'PSPACE', fontsize=13, fontweight='bold', ha='center', color='#7b1fa2')

# 2. BPP (Nested inside PSPACE)
bpp = patches.Circle(
    (0, -0.2), 3.0, 
    facecolor='#e3f2fd', edgecolor='#1565c0', linewidth=2, alpha=0.4
)
ax.add_patch(bpp)
ax.text(0, 2.3, 'BPP', fontsize=12, fontweight='bold', ha='center', color='#1565c0')

# 3. RP and coRP (Overlapping subsets inside BPP)
rp = patches.Circle(
    (-1.0, -0.5), 1.6, 
    facecolor='#c8e6c9', edgecolor='#2e7d32', linewidth=1.5, alpha=0.5
)
corp = patches.Circle(
    (1.0, -0.5), 1.6, 
    facecolor='#ffe0b2', edgecolor='#ef6c00', linewidth=1.5, alpha=0.5
)
ax.add_patch(rp)
ax.add_patch(corp)

# Labels for RP and coRP
ax.text(-1.7, 0.4, 'RP', fontsize=10, fontweight='bold', color='#2e7d32')
ax.text(1.5, 0.4, 'coRP', fontsize=10, fontweight='bold', color='#ef6c00')

# 4. Intersection Region (ZPP / P in the overlap of RP and coRP)
zpp = patches.Ellipse(
    (0, -0.6), 1.0, 0.8, 
    facecolor='#fff9c4', edgecolor='#fbc02d', linewidth=1.5, alpha=0.7
)
ax.add_patch(zpp)
ax.text(0, -0.6, 'ZPP / P', fontsize=9, fontweight='bold', ha='center', color='#827717')

# Layout configuration
ax.set_xlim(-5, 5)
ax.set_ylim(-5, 5)
ax.set_aspect('equal')
plt.axis('off')

# Save directly as an SVG vector file
plt.savefig('complexity_classes.svg', format='svg', bbox_inches='tight')
plt.close()

print("Generated complexity_classes.svg successfully!")
