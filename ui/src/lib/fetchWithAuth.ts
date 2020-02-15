export default (token: string) => async (url: string, options: RequestInit = {}) => {
  const res = await fetch(url, {
    ...options,
    headers: {
      ...(options.headers || {}),
      authorization: `Bearer ${token}`,
    },
  });

  if (res.status !== 200) {
    throw new Error(`API error ${res.statusText}`);
  }

  return res;
};
