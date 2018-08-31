struct map_node_s;

struct map_node_s {
  unsigned hash;
  void *value;
  struct map_node_s *next;
  /* char key[]; */
  /* char value[]; */
};

unsigned
get_hash(struct map_node_s *n)
{
  return n->hash;
}
