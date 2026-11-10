import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(8, 8))

# 1. PSPACE (Outer container)
pspace = plt.Circle((0, 0), 3.5, color='#e1bee7', ec='#7b1fa2', lw=2, alpha=0.3, label='PSPACE')
# 2. BPP (Nested inside PSPACE)
bpp = plt.Circle((0, -0.3), 2.5, color='#bbdefb', ec='#1565c0', lw=2, alpha=0.4, label='BPP')
# 3. Intersecting RP and coRP inside BPP
rp = plt.Circle((-0.8, -0.6), 1.2, color='#c8e6c9', ec='#2e7d32', lw=2, alpha=0.5, label='RP')
corp = plt.Circle((0.8, -0.6), 1.2, color='#ffe0b2', ec='#ef6c00', lw=2, alpha=0.5, label='coRP')

for patch in [pspace, bpp, rp, corp]:
    ax.add_patch(patch)

# Labels
ax.text(0, 2.8, 'PSPACE', fontsize=12, fontweight='bold', ha='center', color='#7b1fa2')
ax.text(0, 1.5, 'BPP', fontsize=11, fontweight='bold', ha='center', color='#1565c0')
ax.text(-0.8, -0.3, 'RP', fontsize=10, ha='center')
ax.text(0.8, -0.3, 'coRP', fontsize=10, ha='center')
ax.text(0, -0.7, 'ZPP / P', fontsize=9, fontweight='bold', ha='center', color='black')

ax.set_xlim(-4, 4)
ax.set_ylim(-4, 4)
ax.set_aspect('equal')
plt.axis('off')

# Save as SVG to track in Git and render on GitHub
plt.savefig('complexity_classes.svg', format='svg', bbox_inches='tight')
