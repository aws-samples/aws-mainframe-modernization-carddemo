import type { ReactNode } from 'react'

interface BadgeProps {
  color: 'green' | 'red' | 'amber' | 'blue' | 'gray'
  dot?: boolean
  children: ReactNode
}

export default function Badge({ color, dot = false, children }: BadgeProps) {
  return (
    <span className={`badge ${color}`}>
      {dot && <span className="dot" />}
      {children}
    </span>
  )
}
