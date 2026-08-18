import react from '@vitejs/plugin-react'
// vitest/config re-exports Vite's defineConfig with the `test` field typed
import { defineConfig } from 'vitest/config'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    host: '0.0.0.0',
    // accept proxied preview hostnames (sandbox preview); dev-only server
    allowedHosts: true,
    proxy: {
      // backend API (Spring Boot) during development
      '/api': {
        target: 'http://localhost:8080',
        configure: (proxy) => {
          proxy.on('proxyReq', (proxyReq, req) => {
            // Preview/edge proxies may replace the Authorization header with
            // their own token; the app also sends the JWT as X-Auth-Token so
            // we can restore it here. Requests without it (e.g. login) get any
            // injected Authorization header stripped.
            const alt = req.headers['x-auth-token']
            if (typeof alt === 'string' && alt.length > 0) {
              proxyReq.setHeader('Authorization', `Bearer ${alt}`)
              proxyReq.removeHeader('X-Auth-Token')
            } else {
              proxyReq.removeHeader('Authorization')
            }
          })
        },
      },
    },
  },
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['src/test/setup.ts'],
  },
})
