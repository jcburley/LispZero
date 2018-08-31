struct cw_s {
  char const *name;
};

static struct cw_s *one;
static struct cw_s *two;

struct cw_s **
get_p(char const *name)
{
  if (name[0]) return &one;
  return &two;
}
