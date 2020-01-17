class Tree:
    def __init__(self, children=None, name=""):
        self.idx = 0
        if children is not None:
            self.children = children
        else:
            self.children = []
        self.name = name

    def __len__(self):
        return 1 + sum(len(child) for child in self.children)

    # Preorder traversal
    def __getitem__(self, val):
        self.idx = 0
        if val == 1:
            return self
        else:
            return self._traverse(val, self)

    def _traverse(self, index, root):
        root.idx += 1
        if root.idx == index:
            return self
        for c in self.children:
            child_trav = c._traverse(index, root)
            if child_trav is not None:
                return child_trav
        return None
            
    def __repr__(self):     
        return "[%s:  %s]" % (self.name, ','.join(repr(child) for child in self.children))

    def add_item(self, val: int, node):
        child = self.__getitem__(val)
        child.children.append(node)
        

sample = Tree([Tree([Tree([], "3"), Tree([], "4")], "2"), Tree([Tree([], "6")], "5")], "Root")

print(sample)

print(sample[6])

sample.add_item(4, Tree([], "nane"))
sample.add_item()
print(sample)

# http://webhome.cs.uvic.ca/~ruskey/Theses/GangLiMScThesis.pdf

